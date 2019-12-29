open Core
open Async
open Binance_ws

val url : Uri.t

val connect :
  ?buf:Bi_outbuf.t ->
  ?hb:Time_ns.Span.t ->
  Stream.t list -> t Pipe.Reader.t Deferred.Or_error.t

val connect_exn :
  ?buf:Bi_outbuf.t ->
  ?hb:Time_ns.Span.t ->
  Stream.t list -> t Pipe.Reader.t Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  ?hb:Time_ns.Span.t ->
  Stream.t list -> f:(t Pipe.Reader.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  ?buf:Bi_outbuf.t ->
  ?hb:Time_ns.Span.t ->
  Stream.t list -> f:(t Pipe.Reader.t -> 'a Deferred.t) ->
  'a Deferred.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = Binance_ws.t Pipe.Reader.t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    ?buf:Bi_outbuf.t ->
    ?hb:Time_ns.Span.t ->
    Stream.t list ->
    (unit -> address Or_error.t Deferred.t) -> t
end
