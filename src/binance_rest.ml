open Core
open Fastrest

open Binance

let url = Uri.make ~scheme:"https" ~host:"api.binance.com" ()

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

let authf srv { key ; secret ; meta = _ } =
  let ps = match srv.params with
    | Form ps -> ps
    | Json (_,_) -> assert false in
  let ps =
    ("timestamp", [Int.to_string (Time_ns.(to_int_ns_since_epoch (now ()) / 1_000_000))]) ::
    ("recvWindow", [Int.to_string 1_000]) ::
    ps in
  let headers = Httpaf.Headers.of_list [
      "X-MBX-APIKEY", key ;
    ] in
  let ps_encoded = Uri.encoded_of_query ps in
  let signature =
    Digestif.SHA256.(hmac_string ~key:secret ps_encoded |> to_hex) in
  let ps = List.rev (("signature", [signature]) :: List.rev ps) in
  { Fastrest.params = Form ps ; headers }

let authf_keyonly { Fastrest.params ; _ } { Fastrest.key ; _ } =
  let headers = Httpaf.Headers.of_list ["X-MBX-APIKEY", key] in
  { Fastrest.params ; headers }

let with_path_and_query ~path ~query uri =
  Uri.with_query (Uri.with_path uri path) query

module ExchangeInfo = struct
  module Sym = struct
    type t = {
      symbol: string ;
      base: string ;
      base_decimals: int ;
      quote: string ;
      quote_decimals: int ;
    } [@@deriving sexp]

    let compare a b = String.compare a.symbol b.symbol
  end

  let sym_encoding =
    let open Json_encoding in
    conv
      (fun { Sym.symbol ; base ; base_decimals ; quote ; quote_decimals } ->
         (), (symbol, base, base_decimals, quote, quote_decimals))
      (fun ((), (symbol, base, base_decimals, quote, quote_decimals)) ->
         { symbol ; base ; base_decimals ; quote ; quote_decimals })
      (merge_objs unit
         (obj5
            (req "symbol" string)
            (req "baseAsset" string)
            (req "baseAssetPrecision" int)
            (req "quoteAsset" string)
            (req "quotePrecision" int)))

  let encoding =
    let open Json_encoding in
    conv
      (fun syms -> (), syms)
      (fun ((), syms) -> syms)
      (merge_objs unit
         (obj1
            (req "symbols" (list sym_encoding))))

  let get =
    Fastrest.get (BinanceError.or_error encoding)
      (Uri.with_path url "api/v1/exchangeInfo")
end

module Depth = struct
  type t = {
    last_update_id : int ;
    bids : Level.t list ;
    asks : Level.t list ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { last_update_id ; bids ; asks } -> (last_update_id, bids, asks))
      (fun (last_update_id, bids, asks) -> { last_update_id ; bids ; asks })
      (obj3
         (req "lastUpdateId" int)
         (req "bids" (list Level.encoding))
         (req "asks" (list Level.encoding)))

  let get ?(limit=100) symbol =
    if not (List.mem ~equal:Int.equal [5; 10; 20; 50; 100; 500; 1000] limit) then
      invalid_argf "Depth.get: invalid limit %d, must belong to [5; \
                    10; 20; 50; 100; 500; 1000]" limit () ;
    Fastrest.get
      (BinanceError.or_error encoding)
      (with_path_and_query url
         ~path:"api/v1/depth"
         ~query:["symbol", [String.uppercase symbol] ;
                 "limit", [string_of_int limit]])
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
      accountType : string ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { makerC ; takerC ; buyerC ; sellerC ;
               trade ; withdraw ; deposit ; timestamp ;
               balances ; accountType } ->
          (makerC, takerC, buyerC, sellerC, trade,
           withdraw, deposit, timestamp, balances, accountType))
        (fun (makerC, takerC, buyerC, sellerC, trade,
              withdraw, deposit, timestamp, balances, accountType) ->
          { makerC ; takerC ; buyerC ; sellerC ;
            trade ; withdraw ; deposit ; timestamp ;
            balances ; accountType })
        (obj10
           (req "makerCommission" int)
           (req "takerCommission" int)
           (req "buyerCommission" int)
           (req "sellerCommission" int)
           (req "canTrade" bool)
           (req "canWithdraw" bool)
           (req "canDeposit" bool)
           (req "updateTime" Ptime.float_encoding)
           (req "balances" (list Balance.encoding))
           (req "accountType" string))

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
               side ; stopPrice = _ ; icebergQty = _ ;
               time ; isWorking = _ } ->
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
      ?(dry_run=false) ~symbol
      ~side ~kind ?timeInForce
      ~qty ?price ?clientOrdID
      ?stopPx ?icebergQty () =
    let params = List.filter_opt [
        Some ("symbol", [symbol]) ;
        Some ("side", [Side.to_string side]) ;
        Some ("type", [OrderType.to_string kind]) ;
        Option.map timeInForce ~f:(fun tif -> "timeInForce", [TimeInForce.to_string tif]) ;
        Some ("quantity", [Printf.sprintf "%.6f" qty]) ;
        Option.map price ~f:(fun p -> "price", [Printf.sprintf "%.6f" p]) ;
        Option.map clientOrdID ~f:(fun id -> "newClientOrderId", [id]) ;
        Option.map stopPx ~f:(fun p -> "stopPrice", [Printf.sprintf "%.6f" p]) ;
        Option.map icebergQty ~f:(fun q -> "icebergQty", [Printf.sprintf "%.6f" q]) ;
      ] in
    let enc =
      let open Json_encoding in
      union [
        case empty (function _ -> None) (function () -> None) ;
        case OrderStatus.order_response_encoding
          Fn.id (fun orderStatus -> Some orderStatus) ;
      ] in
    Fastrest.post_form ~params ~auth:authf (BinanceError.or_error enc)
      (Uri.with_path url ("api/v3/order" ^ if dry_run then "/test" else ""))

  let open_orders symbol =
    Fastrest.get ~auth:authf
      (BinanceError.or_error (Json_encoding.list OrderStatus.encoding))
      Uri.(with_query (with_path url "api/v3/openOrders") ["symbol", [symbol]])

  let account_info () =
    Fastrest.get ~auth:authf
      (BinanceError.or_error AccountInfo.encoding)
      (Uri.with_path url "api/v3/account")

  module Stream = struct
    let encoding =
      let open Json_encoding in
      conv Fn.id Fn.id (obj1 (req "listenKey" string))

    let start () =
      Fastrest.post_form ~auth:authf_keyonly
        (BinanceError.or_error encoding)
        (Uri.with_path url "api/v1/userDataStream")

    let renew ~listenKey =
      Fastrest.put_form
        ~auth:authf_keyonly
        ~params:["listenKey", [listenKey]]
        (BinanceError.or_error Json_encoding.empty)
        (Uri.with_path url "api/v1/userDataStream")

    let close ~listenKey =
      Fastrest.delete ~auth:authf_keyonly
      (BinanceError.or_error Json_encoding.empty)
      Uri.(with_query (with_path url "api/v1/userDataStream")
             ["listenKey", [listenKey]])
  end
end
