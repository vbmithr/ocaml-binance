open Core
open Async

let wrap ?timeout ?(speed=`Quick) n f =
  Alcotest_async.test_case ?timeout n speed begin fun () ->
    f ()
  end

let request ?timeout ?(speed=`Quick) ?auth n req =
  Alcotest_async.test_case ?timeout n speed begin fun () ->
    Fastrest.request ?auth req |>
    Deferred.ignore_m
  end

open Binance
open Binance_rest

let auth =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_BINANCE") with
  | [key; secret] -> Fastrest.auth ~key ~secret ()
  | _ -> assert false

let timeout = Time.Span.of_int_sec 10

let rest = [
  request "exchangeInfo" ~timeout ExchangeInfo.get ;
  request "depth" ~timeout (Depth.get ~limit:5 "BNBBTC") ;
  wrap "stream" ~timeout begin fun () ->
    Fastrest.request ~auth (User.Stream.start ()) >>= fun listenKey ->
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
