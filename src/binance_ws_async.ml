open Async

open Binance
open Binance_ws

let scheme = "https"
let host = "stream.binance.com"
let port = 9443
(* let scheme = "http"
 * let host = "127.0.0.1"
 * let port = 9444 *)
let url = Uri.make ~scheme ~host ~port ()
let src = Logs.Src.create "binance.ws.async"

module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let connect ?(buf=Bi_outbuf.create 4096) streams =
  let url = Uri.with_path url "stream" in
  let url = Uri.with_query url ["streams", [Stream.to_path streams]] in
  Fastws_async.connect_ez url >>= fun (r, w, cleaned_up) ->
  Pipe.close w ;
  let client_read = Pipe.map r ~f:begin fun msg ->
      Yojson_repr.destruct_safe event_encoding (Yojson.Safe.from_string ~buf msg)
    end in
  return (client_read, cleaned_up)

let with_connection ?buf streams ~f =
  connect ?buf streams >>= fun (r, _cleaned_up) ->
  Monitor.protect (fun () -> f r)
    ~finally:(fun () -> Pipe.close_read r ; Deferred.unit)
