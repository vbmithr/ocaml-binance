open Core
open Async

val of_string : ?buf:Bi_outbuf.t -> string -> Binance_ws.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = (Binance_ws.t, unit) Fastws_async.t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    ?buf:Bi_outbuf.t ->
    ?hb:Time_ns.Span.t ->
    (unit -> address Or_error.t Deferred.t) -> t
end
