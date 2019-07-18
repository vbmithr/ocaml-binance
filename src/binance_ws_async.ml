open Core
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

let connect ?(buf=Bi_outbuf.create 4096) ?hb_ns streams =
  let url = Uri.with_path url "stream" in
  let url = Uri.with_query url ["streams", [Stream.to_path streams]] in
  Fastws_async.connect_ez ?hb_ns url >>|
  Result.map ~f:begin fun (r, w, cleaned_up) ->
  Pipe.close w ;
  let client_read = Pipe.map r ~f:begin fun msg ->
      Yojson_repr.destruct_safe encoding (Yojson.Safe.from_string ~buf msg)
    end in
  (client_read, cleaned_up)
  end

let connect_exn ?buf ?hb_ns streams =
  connect ?buf ?hb_ns streams >>= function
  | Error (`Internal exn) -> raise exn
  | Error (`WS e) -> Fastws_async.raise_error e
  | Ok a -> return a

let with_connection ?buf ?hb_ns streams ~f =
  connect ?buf ?hb_ns streams >>= function
  | Error e -> return (Error (e :> [`Internal of exn|`WS of Fastws_async.error|`Cleaned_up]))
  | Ok (r, cleaned_up) ->
    Monitor.protect begin fun () ->
      Deferred.any [
        (cleaned_up >>| fun () -> Error `Cleaned_up) ;
        (f r >>| fun v -> Ok v) ;
      ]
    end ~finally:begin fun () ->
      Pipe.close_read r ; Deferred.unit
    end

let with_connection_exn ?buf ?hb_ns streams ~f =
  with_connection ?buf ?hb_ns streams ~f >>= function
  | Error (`Internal exn) -> raise exn
  | Error (`WS e) -> Fastws_async.raise_error e
  | Error `Cleaned_up -> failwith "cleaned up"
  | Ok a -> return a
