open Core
open Async
open Binance

module Yojson_repr = struct
  include Json_encoding.Make(Json_repr.Yojson)
  let destruct_safe encoding v =
    try destruct encoding v with exn ->
      Format.eprintf "%a@."
        (fun ppf exn -> Json_encoding.print_error ppf exn) exn ;
      raise exn
end

let scheme = "https"
let host = "stream.binance.com"
let port = 9443
let uri = Uri.make ~scheme ~host ~port ()

type topic =
  | Trade
  | Depth

let pp_topic ppf = function
  | Trade -> Format.pp_print_string ppf "trade"
  | Depth -> Format.pp_print_string ppf "depth"

let string_of_topic = Fmt.to_to_string pp_topic
let topic_of_string = function
  | "trade" -> Trade
  | "depth" -> Depth
  | _ -> invalid_arg "topic_of_string"

type stream = {
  topic : topic ;
  symbol : string ;
}

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

let ssl_config = Conduit_async.Ssl.configure ~version:Tlsv1_2 ()
let open_connection ?(buf=Bi_outbuf.create 4096) ?connected ?log streams =
  let uri = Uri.with_path uri "stream" in
  let uri = Uri.with_query uri ["streams", [path_of_streams streams]] in
  let uri_str = Uri.to_string uri in
  let endp = Host_and_port.create ~host ~port in
  let client_r, client_w = Pipe.create () in
  let cleanup r w ws_r ws_w =
    Option.iter log ~f:(fun log ->
        Log.debug log "[WS] post-disconnection cleanup") ;
    Pipe.close ws_w ;
    Pipe.close_read ws_r ;
    Deferred.all_unit [Reader.close r ; Writer.close w ] ;
  in
  let tcp_fun s r w =
    Option.iter log ~f:(fun log ->
        Log.info log "[WS] connecting to %s" uri_str);
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss" then
       Conduit_async_ssl.(ssl_connect ssl_config r w)
     else return (r, w)) >>= fun (ssl_r, ssl_w) ->
    let ws_r, ws_w =
      Websocket_async.client_ez ?log
        ~heartbeat:(Time_ns.Span.of_int_sec 25) uri s ssl_r ssl_w in
    don't_wait_for begin
      Deferred.all_unit
        [ Reader.close_finished r ; Writer.close_finished w ] >>= fun () ->
      cleanup ssl_r ssl_w ws_r ws_w
    end ;
    Option.iter connected ~f:(fun c -> Condition.broadcast c ());
    Pipe.transfer ws_r client_w ~f:begin fun str ->
      Yojson_repr.destruct_safe event_encoding (Yojson.Safe.from_string ~buf str)
    end
  in
  let rec loop () = begin
    Monitor.try_with_or_error
      ~name:"open_connection"
      ~extract_exn:false
      begin fun () ->
        Tcp.(with_connection (Where_to_connect.of_host_and_port endp) tcp_fun)
      end >>| function
    | Ok () -> Option.iter log ~f:(fun log ->
        Log.error log "[WS] connection to %s terminated" uri_str);
    | Error err -> Option.iter log ~f:(fun log ->
        Log.error log "[WS] connection to %s raised %s"
          uri_str (Error.to_string_hum err))
  end >>= fun () ->
    if Pipe.is_closed client_r then Deferred.unit
    else Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>= loop
  in
  don't_wait_for @@ loop ();
  client_r
