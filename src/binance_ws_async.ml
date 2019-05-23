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
let uri = Uri.make ~scheme ~host ~port ()
let src = Logs.Src.create "binance.ws.async"

module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let connect ?(buf=Bi_outbuf.create 4096) ?connected streams =
  let uri = Uri.with_path uri "stream" in
  let uri = Uri.with_query uri ["streams", [Stream.to_path streams]] in
  let client_r, client_w = Pipe.create () in
  let inner r w =
    let cleanup r w =
      Pipe.closed client_r >>= fun () ->
      Log_async.debug begin fun m ->
        m "post-disconnection cleanup"
      end >>| fun () ->
      Pipe.close w ;
      Pipe.close_read r
    in
    don't_wait_for (cleanup r w) ;
    Option.iter connected ~f:(fun c -> Condition.broadcast c ());
    Pipe.transfer r client_w ~f:begin fun str ->
      Yojson_repr.destruct_safe event_encoding (Yojson.Safe.from_string ~buf str)
    end
  in
  let rec loop () = begin
    Monitor.try_with_or_error ~extract_exn:false
      begin fun () ->
        Log_async.info begin fun m ->
          m "connecting to %a" Uri.pp_hum uri
        end >>= fun () ->
        Fastws_async.with_connection_ez uri ~f:inner
      end >>= function
    | Ok () ->
      Log_async.err begin fun m ->
        m "connection to %a terminated" Uri.pp_hum uri
      end
    | Error err ->
      Log_async.err begin fun m ->
        m "connection to %a raised %a" Uri.pp_hum uri Error.pp err
      end
  end >>= fun () ->
    if Pipe.is_closed client_r then Deferred.unit
    else Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>= loop
  in
  don't_wait_for @@ loop ();
  client_r

let with_connection ?buf ?connected streams ~f =
  let r = connect ?buf ?connected streams in
  Monitor.protect (fun () -> f r)
    ~finally:(fun () -> Pipe.close_read r ; Deferred.unit)
