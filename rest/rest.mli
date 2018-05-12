open Core
open Async
open Binance

module Depth : sig
  type t = {
    last_update_id : int ;
    bids : Depth.level list ;
    asks : Depth.level list ;
  }

  val get :
    ?limit:int -> string -> (Cohttp.Response.t * t) Or_error.t Deferred.t
end
