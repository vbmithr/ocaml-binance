open Core
open Async

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
    | Error err ->
      let msg = (Rest.BinanceError.to_string err) in
      printf "%s" msg ;
      failwith msg
  end

let rest = [
  wrap "get" (fun () -> Rest.Depth.get ~limit:5 "BNBBTC") ;
  wrap "user" begin fun () ->
    let open Deferred.Result.Monad_infix in
    Rest.User.Stream.start ~key:cfg.key () >>=
    Rest.User.Stream.close ~key:cfg.key
  end ;
  wrap "open_orders" begin fun () ->
    Rest.User.open_orders ~key:cfg.key ~secret:cfg.secret "BNBBTC"
  end ;
  wrap "account_info" begin fun () ->
    let open Deferred.Result.Monad_infix in
    Rest.User.account_info ~key:cfg.key ~secret:cfg.secret () >>| fun ai ->
    printf "%s" (Rest.User.AccountInfo.to_string ai)
  end ;
  wrap "fake_trade" begin fun () ->
    let open Deferred.Result.Monad_infix in
    Rest.User.order
      ~dry_run:true
      ~key:cfg.key ~secret:cfg.secret
      ~symbol:"BNBBTC"
      ~side:`Buy
      ~kind:OrderType.Market
      ~qty:2. () >>|
    Option.iter ~f:(fun ordStatus ->
        printf "%s" (Rest.User.OrderStatus.to_string ordStatus))
  end

]

let () =
  Alcotest.run "binance" [
    "rest", rest ;
  ]
