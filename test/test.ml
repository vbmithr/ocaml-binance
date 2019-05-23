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

let wrap ?(speed=`Quick) n f =
  Alcotest_async.test_case n speed begin fun () ->
    f () >>= function
    | Ok _ -> Deferred.unit
    | Error (Fastrest.Http _e) ->
      assert false
    | Error (App err) ->
      let msg = (Binance_rest.BinanceError.to_string err) in
      printf "%s" msg ;
      failwith msg
  end

open Binance_rest

let rest = [
  wrap "get" (fun () ->
      Fastrest.request (Depth.get ~limit:5 "BNBBTC")) ;
  (* wrap "user" begin fun () ->
   *   let open Deferred.Result.Monad_infix in
   *   User.Stream.start ~key:cfg.key () >>=
   *   User.Stream.close ~key:cfg.key
   * end ;
   * wrap "open_orders" begin fun () ->
   *   User.open_orders ~key:cfg.key ~secret:cfg.secret "BNBBTC"
   * end ;
   * wrap "account_info" begin fun () ->
   *   let open Deferred.Result.Monad_infix in
   *   User.account_info ~key:cfg.key ~secret:cfg.secret () >>| fun ai ->
   *   printf "%s" (User.AccountInfo.to_string ai)
   * end ;
   * wrap "fake_trade" begin fun () ->
   *   let open Deferred.Result.Monad_infix in
   *   User.order
   *     ~dry_run:true
   *     ~key:cfg.key ~secret:cfg.secret
   *     ~symbol:"BNBBTC"
   *     ~side:`Buy
   *     ~kind:OrderType.Market
   *     ~qty:2. () >>|
   *   Option.iter ~f:(fun ordStatus ->
   *       printf "%s" (User.OrderStatus.to_string ordStatus))
   * end *)

]

let () =
  Logs.set_level (Some Debug) ;
  Alcotest.run "binance" [
    "rest", rest ;
  ]
