open Core
open Async
module C = Cohttp
open Cohttp_async

open Binance

module Yojson_repr = struct
  include Json_encoding.Make(Json_repr.Yojson)
  let destruct_safe encoding v =
    try destruct encoding v with exn ->
      Format.eprintf "%a@."
        (fun ppf exn -> Json_encoding.print_error ppf exn) exn ;
      raise exn
end

let url = Uri.make ~scheme:"https" ~host:"api.binance.com" ()
let ssl_config =
  Conduit_async_ssl.Cfg.create
    ~name:"Binance REST"
    ~hostname:"api.binance.com"
    ()

module HTTPError = struct
  type t = {
    code : int ;
    msg : string ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { code ; msg } -> (code, msg))
      (fun (code, msg) -> { code ; msg })
      (obj2
         (req "code" int)
         (req "msg" string))
end

let call
    ?extract_exn
    ?buf
    ?log
    ?(span=Time_ns.Span.of_int_sec 1)
    ?(max_tries=3)
    ?params
    ?key
    ?sign
    ~meth
    path =
  let url = Uri.with_path url path in
  let url = Option.value_map params ~default:url ~f:(Uri.with_query url) in
  (* begin match log, body_str with
   *   | Some log, Some body_str ->
   *     Log.debug log "%s %s -> %s" (show_verb verb) path body_str
   *   | _ -> ()
   * end ; *)
  let headers = C.Header.init () in
  let headers = Option.value_map params ~default:headers ~f:(fun _ ->
      C.Header.add headers "content-type" "application/x-www-form-urlencoded") in
  let headers = Option.value_map key ~default:headers
      ~f:(fun key -> C.Header.add headers "X-MBX-APIKEY" key) in
  let body = match meth with
    | `GET -> None
    | #C.Code.meth ->
      Option.map params ~f:(fun ps -> Body.of_string (Uri.encoded_of_query ps)) in
  let call () = match meth with
    | `GET -> Client.get ~ssl_config ~headers url
    | `POST -> Client.post ~ssl_config ~headers ~chunked:false ?body url
    | `PUT -> Client.put ~ssl_config ~headers ~chunked:false ?body url
    | `DELETE -> Client.delete ~ssl_config ~headers ~chunked:false ?body url
    | #C.Code.meth as m ->
      invalid_argf "Unsupported HTTP method %s" (C.Code.string_of_method m) () in
  let rec inner_exn try_id =
    call () >>= fun (resp, body) ->
    Body.to_string body >>= fun body_str ->
    let status = Response.status resp in
    let status_code = C.Code.code_of_status status in
    if C.Code.is_success status_code then
      return (resp, Yojson.Safe.from_string ?buf body_str)
    else if C.Code.is_server_error status_code then begin
      let status_code_str = C.Code.string_of_status status in
      Option.iter log ~f:begin fun log ->
        Log.error log "%s %s: %s" (C.Code.string_of_method meth) path status_code_str
      end ;
      Clock_ns.after span >>= fun () ->
      if try_id >= max_tries then
        failwithf "%s %s: %s" (C.Code.string_of_method meth) path status_code_str ()
      else inner_exn @@ succ try_id
    end
    else
      failwithf "%s: %s" (Uri.to_string url) (C.Code.string_of_status status) ()
  in
  Monitor.try_with_or_error ?extract_exn (fun () -> inner_exn 0)

module Depth = struct
  type t = {
    last_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { last_update_id ; bids ; asks } -> (last_update_id, bids, asks))
      (fun (last_update_id, bids, asks) -> { last_update_id ; bids ; asks })
      (obj3
         (req "lastUpdateId" int)
         (req "bids" (list Level.encoding))
         (req "asks" (list Level.encoding)))

  let get ?log ?(limit=100) symbol =
    if not (List.mem ~equal:Int.equal [5; 10; 20; 50; 100; 500; 1000] limit) then
      invalid_argf "Depth.get: invalid limit %d, must belong to [5; \
                    10; 20; 50; 100; 500; 1000]" limit () ;
    call ?log ~params:["symbol", [String.uppercase symbol] ;
                  "limit", [string_of_int limit] ;
                 ] ~meth:`GET "api/v1/depth" >>|
    Or_error.map ~f:begin fun (resp, depth) ->
      resp, Yojson_repr.destruct_safe encoding depth
    end
end
