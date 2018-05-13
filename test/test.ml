open Core
open Async

open Binance

let wrap ?(speed=`Quick) n f =
  Alcotest_async.test_case n speed begin fun () ->
    f () >>= function
    | Ok _ -> Deferred.unit
    | Error err -> failwith (Error.to_string_hum err)
  end

let rest = [
  wrap "get" (fun () -> Rest.Depth.get ~limit:5 "BNBBTC")
]

let () =
  Alcotest.run "binance" [
    "rest", rest ;
  ]
