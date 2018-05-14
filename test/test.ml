open Core
open Async
open Log.Global

open Binance

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

let wrap ?(speed=`Quick) n f =
  Alcotest_async.test_case n speed begin fun () ->
    f () >>= function
    | Ok _ -> Deferred.unit
    | Error err -> failwith (Error.to_string_hum err)
  end

let rest = [
  wrap "get" (fun () -> Rest.Depth.get ~limit:5 "BNBBTC") ;
  wrap "user" begin fun () ->
    Rest.User.Stream.start ~key:cfg.key () >>|
    Or_error.map ~f:begin fun (_resp, listenKey) ->
      Rest.User.Stream.close ~key:cfg.key listenKey
    end
  end ;
  wrap "open_orders" begin fun () ->
    Rest.User.open_orders ~key:cfg.key ~secret:cfg.secret "BNBBTC"
  end ;
  wrap "account_info" begin fun () ->
    Rest.User.account_info ~key:cfg.key ~secret:cfg.secret () >>|
    Or_error.map ~f:begin fun (_resp, ai) ->
      printf "%s" (Rest.User.AccountInfo.to_string ai)
    end
  end ;
  wrap "fake_trade" begin fun () ->
    Rest.User.order
      ~log:(Lazy.force log)
      ~dry_run:true
      ~key:cfg.key ~secret:cfg.secret
      ~symbol:"BNBBTC"
      ~side:`Buy
      ~kind:OrderType.Market
      ~qty:2. () >>|
    Or_error.map ~f:begin fun (_resp, ordStatus) ->
      Option.iter ordStatus ~f:(fun ordStatus ->
          printf "%s" (Rest.User.OrderStatus.to_string ordStatus))
    end
  end

]

let () =
  Alcotest.run "binance" [
    "rest", rest ;
  ]
