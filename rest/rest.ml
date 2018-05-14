open Core
open Async
module C = Cohttp
open Cohttp_async

open Binance

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
    ?(params=[])
    ?key
    ?secret
    ~meth
    path =
  let url = Uri.with_path url path in
  let headers = match params with
    | [] -> C.Header.init ()
    | _ -> C.Header.init_with "content-type"
             "application/x-www-form-urlencoded" in
  (* begin match log, body_str with
   *   | Some log, Some body_str ->
   *     Log.debug log "%s %s -> %s" (show_verb verb) path body_str
   *   | _ -> ()
   * end ; *)
  let headers = Option.value_map key ~default:headers
      ~f:(fun key -> C.Header.add headers "X-MBX-APIKEY" key) in
  let params = Option.value_map secret ~default:params ~f:begin fun secret ->
      let key = Bytes.unsafe_of_string_promise_no_mutation secret in
      let params =
        ("timestamp", [Int.to_string (Time_ns.(to_int_ns_since_epoch (now ()) / 1_000_000))]) ::
        ("recvWindow", [Int.to_string 1_000]) ::
        params in
      let ps_encoded =
        Bytes.unsafe_of_string_promise_no_mutation
          (Uri.encoded_of_query params) in
      let `Hex signature =
        Hex.of_string (Bytes.unsafe_to_string
                         (Digestif.SHA256.Bytes.hmac ~key ps_encoded)) in
      List.rev (("signature", [signature]) :: List.rev params)
    end in
  let body = match meth with
    | `GET -> None
    | #C.Code.meth -> Some (Body.of_string (Uri.encoded_of_query params)) in
  let call () = match meth with
    | `GET -> Client.get ~ssl_config ~headers (Uri.with_query url params)
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
      failwithf "%s: %s %s"
        (Uri.to_string url) (C.Code.string_of_status status) body_str ()
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

  let get ?buf ?log ?(limit=100) symbol =
    if not (List.mem ~equal:Int.equal [5; 10; 20; 50; 100; 500; 1000] limit) then
      invalid_argf "Depth.get: invalid limit %d, must belong to [5; \
                    10; 20; 50; 100; 500; 1000]" limit () ;
    call ?buf ?log ~params:["symbol", [String.uppercase symbol] ;
                  "limit", [string_of_int limit] ;
                 ] ~meth:`GET "api/v1/depth" >>|
    Or_error.map ~f:begin fun (resp, depth) ->
      resp, Yojson_repr.destruct_safe encoding depth
    end
end

module User = struct
  module Balance = struct
    type t = {
      asset : string ;
      free : float ;
      locked : float ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { asset ; free ; locked } -> (asset, free, locked))
        (fun (asset, free, locked) -> { asset ; free ; locked })
        (obj3
           (req "asset" string)
           (req "free" float_as_string)
           (req "locked" float_as_string))
  end

  module AccountInfo = struct
    type t = {
      makerC : int ;
      takerC : int ;
      buyerC : int ;
      sellerC : int ;
      trade : bool ;
      withdraw : bool ;
      deposit : bool ;
      timestamp : Ptime.t ;
      balances : Balance.t list ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { makerC ; takerC ; buyerC ; sellerC ;
               trade ; withdraw ; deposit ; timestamp ;
               balances } ->
          (makerC, takerC, buyerC, sellerC, trade,
           withdraw, deposit, timestamp, balances))
        (fun (makerC, takerC, buyerC, sellerC, trade,
              withdraw, deposit, timestamp, balances) ->
          { makerC ; takerC ; buyerC ; sellerC ;
            trade ; withdraw ; deposit ; timestamp ;
            balances })
        (obj9
           (req "makerCommission" int)
           (req "takerCommission" int)
           (req "buyerCommission" int)
           (req "sellerCommission" int)
           (req "canTrade" bool)
           (req "canWithdraw" bool)
           (req "canDeposit" bool)
           (req "updateTime" Ptime.float_encoding)
           (req "balances" (list Balance.encoding)))

    let pp ppf t =
      Json_repr.(pp (module Yojson) ppf (Yojson_repr.construct encoding t))
    let to_string = Fmt.to_to_string pp
  end

  module OrderStatus = struct
    type t = {
      symbol : string ;
      orderId : int ;
      clientOrderId : string ;
      price : float ;
      origQty : float ;
      executedQty : float ;
      ordStatus : OrderStatus.t ;
      timeInForce : TimeInForce.t ;
      ordType : OrderType.t ;
      side : Side.t ;
      stopPrice : float ;
      icebergQty : float ;
      time : Ptime.t ;
      isWorking : bool ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { symbol ; orderId ; clientOrderId ;
               price ; origQty ; executedQty ;
               ordStatus ; timeInForce ; ordType ;
               side ; stopPrice ; icebergQty ;
               time ; isWorking } ->
          ((symbol, orderId, clientOrderId, price, origQty,
            executedQty, ordStatus, timeInForce, ordType, side),
           (stopPrice, icebergQty, time, isWorking)))
        (fun ((symbol, orderId, clientOrderId, price, origQty,
               executedQty, ordStatus, timeInForce, ordType, side),
              (stopPrice, icebergQty, time, isWorking)) ->
          { symbol ; orderId ; clientOrderId ;
            price ; origQty ; executedQty ;
            ordStatus ; timeInForce ; ordType ;
            side ; stopPrice ; icebergQty ;
            time ; isWorking })
        (merge_objs
           (obj10
              (req "symbol" string)
              (req "orderId" int)
              (req "clientOrderId" string)
              (req "price" float_as_string)
              (req "origQty" float_as_string)
              (req "executedQty" float_as_string)
              (req "status" OrderStatus.encoding)
              (req "timeInForce" TimeInForce.encoding)
              (req "type" OrderType.encoding)
              (req "side" Side.encoding))
           (obj4
              (req "stopPrice" float_as_string)
              (req "icebergQty" float)
              (req "time" Ptime.float_encoding)
              (req "isWorking" bool)))
  end

  let open_orders ?buf ?log ~key ~secret symbol =
    call ?buf ?log ~meth:`GET ~key ~secret ~params:[
      "symbol", [symbol] ;
    ] "api/v3/openOrders" >>|
    Or_error.map ~f:begin fun (resp, listenKey) ->
      resp, Yojson_repr.destruct_safe
        (Json_encoding.list OrderStatus.encoding) listenKey
    end

  let account_info ?buf ?log ~key ~secret () =
    call ?buf ?log ~meth:`GET ~key ~secret "api/v3/account" >>|
    Or_error.map ~f:begin fun (resp, listenKey) ->
      resp, Yojson_repr.destruct_safe AccountInfo.encoding listenKey
    end

  module Stream = struct
    let encoding =
      let open Json_encoding in
      conv Fn.id Fn.id (obj1 (req "listenKey" string))

    let start ?buf ?log ~key () =
      call ?buf ?log ~meth:`POST ~key "api/v1/userDataStream" >>|
      Or_error.map ~f:begin fun (resp, listenKey) ->
        resp, Yojson_repr.destruct_safe encoding listenKey
      end

    let renew ?buf ?log ~key listenKey =
      call ?buf ?log ~meth:`PUT ~key
        ~params:["listenKey", [listenKey]] "api/v1/userDataStream" >>|
      Or_error.map ~f:begin fun (resp, empty) ->
        resp, Yojson_repr.destruct_safe Json_encoding.empty empty
      end

    let close ?buf ?log ~key listenKey =
      call ?buf ?log ~meth:`DELETE ~key
        ~params:["listenKey", [listenKey]] "api/v1/userDataStream" >>|
      Or_error.map ~f:begin fun (resp, empty) ->
        resp, Yojson_repr.destruct_safe Json_encoding.empty empty
      end
  end
end
