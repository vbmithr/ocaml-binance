open Core
open Async

open Binance

let scheme = "https"
let host = "stream.binance.com"
let port = 9443
(* let scheme = "http"
 * let host = "127.0.0.1"
 * let port = 9444 *)
let uri = Uri.make ~scheme ~host ~port ()
let src = Logs.Src.create "binance.ws"

type topic =
  | Trade
  | Depth

let pp_topic ppf = function
  | Trade -> Format.pp_print_string ppf "trade"
  | Depth -> Format.pp_print_string ppf "depth"

let topic_of_string = function
  | "trade" -> Trade
  | "depth" -> Depth
  | _ -> invalid_arg "topic_of_string"

type stream = {
  topic : topic ;
  symbol : string ;
}

let create_stream ~topic ~symbol =
  { topic ; symbol = String.lowercase symbol }

let pp_stream ppf { topic ; symbol } =
  Format.pp_print_string ppf symbol ;
  Format.pp_print_char ppf '@' ;
  pp_topic ppf topic

let string_of_stream = Fmt.to_to_string pp_stream
let stream_of_string s =
  match String.split s ~on:'@' with
  | [symbol; topic] -> { topic = topic_of_string topic ; symbol }
  | _ -> invalid_arg "stream_of_string"

let path_of_streams streams =
  String.concat ~sep:"/" (List.map streams ~f:string_of_stream)

type event =
  | Trade of Trade.t
  | Depth of Depth.t

let trade t = Trade t
let depth d = Depth d

let event_encoding =
  let open Json_encoding in
  union [
    case Trade.encoding (function Trade t -> Some t | _ -> None) trade ;
    case Depth.encoding (function Depth d -> Some d | _ -> None) depth ;
  ]

let event_encoding =
  let open Json_encoding in
  let to_json = function
      | Trade t -> (t.symbol ^ "@trade", Trade t)
      | Depth d -> (d.symbol ^ "@depth", Depth d) in
  let of_json (_, data) = data in
  conv to_json of_json
    (obj2
       (req "stream" string)
       (req "data" event_encoding))

let connect ?(buf=Bi_outbuf.create 4096) ?connected streams =
  let uri = Uri.with_path uri "stream" in
  let uri = Uri.with_query uri ["streams", [path_of_streams streams]] in
  let client_r, client_w = Pipe.create () in
  let inner r w =
    let cleanup r w =
      Pipe.closed client_r >>= fun () ->
      Logs_async.debug ~src begin fun m ->
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
        Logs_async.info ~src begin fun m ->
          m "connecting to %a" Uri.pp_hum uri
        end >>= fun () ->
        Fastws_async.with_connection_ez uri ~f:inner
      end >>= function
    | Ok () ->
      Logs_async.err ~src begin fun m ->
        m "connection to %a terminated" Uri.pp_hum uri
      end
    | Error err ->
      Logs_async.err ~src begin fun m ->
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
