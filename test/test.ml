open Core
open Async

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "BINANCE"

let wrap ?timeout ?(speed=`Quick) n f =
  Alcotest_async.test_case ?timeout n speed begin fun () ->
    f () >>= function
    | Ok _ -> Deferred.unit
    | Error e ->  Alcotest.fail (Error.to_string_hum e)
  end

let request ?timeout ?(speed=`Quick) ?auth n req =
  Alcotest_async.test_case ?timeout n speed begin fun () ->
    Fastrest.request ?auth req >>= function
    | Ok _ -> Deferred.unit
    | Error e ->  Alcotest.fail (Error.to_string_hum e)
  end

open Binance
open Binance_rest

let auth = Fastrest.auth ~key:cfg.key ~secret:cfg.secret ()

let timeout = Time.Span.of_int_sec 10

let rest = [
  request "exchangeInfo" ~timeout ExchangeInfo.get ;
  request "depth" ~timeout (Depth.get ~limit:5 "BNBBTC") ;
  wrap "stream" ~timeout begin fun () ->
    Fastrest.request ~auth (User.Stream.start ()) >>= function
    | Error msg -> return (Error msg)
    | Ok listenKey ->
      Fastrest.request ~auth (User.Stream.close ~listenKey)
  end ;
  request "open_orders" ~timeout ~auth (User.open_orders "BNBBTC") ;
  request "account_info" ~timeout ~auth (User.account_info ()) ;
  request "myTrades" ~timeout ~auth (User.myTrades "ZILBTC") ;
  request "fake_trade" ~timeout ~auth
    (User.order
       ~dry_run:true
       ~symbol:"BNBBTC"
       ~side:Buy
       ~kind:OrderType.Market
       ~qty:2. ())
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level ~all:true (Some Debug) ;
  Alcotest.run "binance" [
    "rest", rest ;
  ]
