open Core
open Async
open Binance

module Depth : sig
  type t = {
    last_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  }

  val get :
    ?log:Log.t -> ?limit:int ->
    string -> (Cohttp.Response.t * t) Or_error.t Deferred.t
end
