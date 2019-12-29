open Core
open Async

open Binance
open Binance_ws

let url =
  Uri.make
    ~scheme:"https"
    ~host:"stream.binance.com"
    ~port:9443
    ~path:"stream" ()

let src = Logs.Src.create "binance.ws.async"

module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

module T = struct
  type t = Binance_ws.t Pipe.Reader.t

  module Address = Uri_sexp

  let is_closed r =
    Pipe.is_closed r

  let close r =
    Pipe.close_read r ;
    Deferred.unit

  let close_finished r =
    Pipe.closed r
end
include T

let connect ?(buf=Bi_outbuf.create 4096) ?hb streams =
  let url = Uri.with_query url ["streams", [Stream.to_path streams]] in
  Deferred.Or_error.map (Fastws_async.connect ?hb url)
    ~f:begin fun { r; w; _ } ->
      Pipe.close w ;
      let client_read = Pipe.map r ~f:begin fun msg ->
          Yojson_repr.destruct_safe encoding (Yojson.Safe.from_string ~buf msg)
        end in
      client_read
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay ?buf ?hb streams =
    create ~server_name ?on_event ?retry_delay
      ~connect:(fun _ -> connect ?buf ?hb streams)
end

let connect_exn ?buf ?hb streams =
  connect ?buf ?hb streams >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?buf ?hb streams ~f =
  Deferred.Or_error.bind (connect ?buf ?hb streams) ~f:begin fun r ->
    Monitor.protect (fun () -> (f r >>| fun v -> Ok v))
      ~finally:(fun () -> Pipe.close_read r ; Deferred.unit)
  end

let with_connection_exn ?buf ?hb streams ~f =
  with_connection ?buf ?hb streams ~f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
