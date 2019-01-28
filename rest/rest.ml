open Core
open Async
module C = Cohttp
open Cohttp_async

open Binance

let url = Uri.make ~scheme:"https" ~host:"api.binance.com" ()
let ssl_config = Conduit_async.Ssl.configure ~name:"Binance REST" ()
let src = Logs.Src.create "binance.rest"

module BinanceError = struct
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

  let or_error enc =
    let open Json_encoding in
    union [
      case encoding
        (function Ok _ -> None | Error e -> Some e)
        (function e -> Error e) ;
      case enc
        (function Ok v -> Some v | Error _ -> None)
        (function v -> Ok v) ;
    ]

  let pp ppf t =
    Json_repr.(pp (module Yojson) ppf (Yojson_repr.construct encoding t))
  let to_string = Fmt.to_to_string pp
end

let call
    ?extract_exn
    ?buf
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
      let signature =
        Digestif.SHA256.(hmac_bytes ~key ps_encoded |> to_hex) in
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
    Logs_async.debug ~src (fun m -> m "-> %s" body_str) >>= fun () ->
    let status = Response.status resp in
    let status_code = C.Code.code_of_status status in
    if C.Code.is_server_error status_code then begin
      let status_code_str = C.Code.string_of_status status in
      Logs_async.err ~src begin fun m ->
        m "%s %s: %s" (C.Code.string_of_method meth) path status_code_str
      end >>= fun () ->
      Clock_ns.after span >>= fun () ->
      if try_id >= max_tries then
        return (resp, Yojson.Safe.from_string ?buf body_str)
      else inner_exn @@ succ try_id
    end
    else
      return (resp, Yojson.Safe.from_string ?buf body_str)
  in
  inner_exn 0

let destruct_resp enc (resp, json) =
  if Cohttp.(Code.is_error (Code.code_of_status resp.Response.status)) then
    Error (Yojson_repr.destruct_safe BinanceError.encoding json)
  else
    Ok (Yojson_repr.destruct_safe enc json)

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

  let get ?buf ?(limit=100) symbol =
    if not (List.mem ~equal:Int.equal [5; 10; 20; 50; 100; 500; 1000] limit) then
      invalid_argf "Depth.get: invalid limit %d, must belong to [5; \
                    10; 20; 50; 100; 500; 1000]" limit () ;
    call ?buf ~params:["symbol", [String.uppercase symbol] ;
                  "limit", [string_of_int limit] ;
                           ] ~meth:`GET "api/v1/depth" >>|
    destruct_resp encoding
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

    let base_status_obj =
      let open Json_encoding in
      obj10
        (req "symbol" string)
        (req "orderId" int)
        (req "clientOrderId" string)
        (req "price" float_as_string)
        (req "origQty" float_as_string)
        (req "executedQty" float_as_string)
        (req "status" OrderStatus.encoding)
        (req "timeInForce" TimeInForce.encoding)
        (req "type" OrderType.encoding)
        (req "side" Side.encoding)

    let order_response_encoding =
      let open Json_encoding in
      conv
        (fun { symbol ; orderId ; clientOrderId ;
               price ; origQty ; executedQty ;
               ordStatus ; timeInForce ; ordType ;
               side ; stopPrice ; icebergQty ;
               time ; isWorking } ->
          ((symbol, orderId, clientOrderId, price, origQty,
            executedQty, ordStatus, timeInForce, ordType, side),
           time))
        (fun ((symbol, orderId, clientOrderId, price, origQty,
               executedQty, ordStatus, timeInForce, ordType, side),
              time) -> { symbol ; orderId ; clientOrderId ;
                         price ; origQty ; executedQty ;
                         ordStatus ; timeInForce ; ordType ;
                         side ; stopPrice = 0.; icebergQty = 0.;
                         time ; isWorking = false})
        (merge_objs base_status_obj
           (obj1 (req "transactTime" Ptime.float_encoding)))

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
        (merge_objs base_status_obj
           (obj4
              (req "stopPrice" float_as_string)
              (req "icebergQty" float)
              (req "time" Ptime.float_encoding)
              (req "isWorking" bool)))

    let pp ppf t =
      Json_repr.(pp (module Yojson) ppf (Yojson_repr.construct encoding t))
    let to_string = Fmt.to_to_string pp
  end

  let order
      ?buf
      ?(dry_run=false)
      ~key ~secret ~symbol
      ~side ~kind ?timeInForce
      ~qty ?price ?clientOrdID
      ?stopPx ?icebergQty () =
    let params = List.filter_opt [
        Some ("symbol", [symbol]) ;
        Some ("side", [Side.to_string side]) ;
        Some ("type", [OrderType.to_string kind]) ;
        Option.map timeInForce(fun tif -> "timeInForce", [TimeInForce.to_string tif]) ;
        Some ("quantity", [Printf.sprintf "%.6f" qty]) ;
        Option.map price (fun p -> "price", [Printf.sprintf "%.6f" p]) ;
        Option.map clientOrdID (fun id -> "newClientOrderId", [id]) ;
        Option.map stopPx (fun p -> "stopPrice", [Printf.sprintf "%.6f" p]) ;
        Option.map icebergQty (fun q -> "icebergQty", [Printf.sprintf "%.6f" q]) ;
      ] in
    let enc =
      let open Json_encoding in
      union [
        case empty (function _ -> None) (function () -> None) ;
        case OrderStatus.order_response_encoding
          Fn.id (fun orderStatus -> Some orderStatus) ;
      ] in
    call ?buf ~meth:`POST ~key ~secret ~params
      ("api/v3/order" ^ if dry_run then "/test" else "") >>|
      destruct_resp enc

  let open_orders ?buf ~key ~secret symbol =
    call ?buf ~meth:`GET ~key ~secret ~params:[
      "symbol", [symbol] ;
    ] "api/v3/openOrders" >>|
    destruct_resp (Json_encoding.list OrderStatus.encoding)

  let account_info ?buf ~key ~secret () =
    call ?buf ~meth:`GET ~key ~secret "api/v3/account" >>|
    destruct_resp AccountInfo.encoding

  module Stream = struct
    let encoding =
      let open Json_encoding in
      conv Fn.id Fn.id (obj1 (req "listenKey" string))

    let start ?buf ~key () =
      call ?buf ~meth:`POST ~key "api/v1/userDataStream" >>|
      destruct_resp encoding

    let renew ?buf ~key listenKey =
      call ?buf ~meth:`PUT ~key
        ~params:["listenKey", [listenKey]] "api/v1/userDataStream" >>|
      destruct_resp Json_encoding.empty

    let close ?buf ~key listenKey =
      call ?buf ~meth:`DELETE ~key
        ~params:["listenKey", [listenKey]] "api/v1/userDataStream" >>|
      destruct_resp Json_encoding.empty
  end
end
