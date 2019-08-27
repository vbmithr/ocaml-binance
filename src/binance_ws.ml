open Sexplib.Std
open Binance

module Trade = struct
  type t = {
    event_ts : Ptime.t ;
    trade_ts : Ptime.t ;
    symbol : string ;
    side : [`Buy | `Sell] ;
    id : int64 ;
    buyer_order_id : int64 ;
    seller_order_id : int64 ;
    p : float ;
    q : float ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; trade_ts ; symbol ; side ; id ; p ; q ;
             buyer_order_id ; seller_order_id } ->
        (), (event_ts, trade_ts, symbol, id, p, q,
             buyer_order_id, seller_order_id, side = `Buy))
      (fun ((), (event_ts, trade_ts, symbol, id, p, q,
                 buyer_order_id, seller_order_id, buyer_is_mm)) ->
        let side = if buyer_is_mm then `Sell else `Buy in
        { event_ts ; trade_ts ; symbol ; side ; id ; p ; q ;
          buyer_order_id ; seller_order_id })
      (merge_objs unit
         (obj9
            (req "E" Ptime.encoding)
            (req "T" Ptime.encoding)
            (req "s" string)
            (req "t" int53)
            (req "p" safe_float)
            (req "q" safe_float)
            (req "b" int53)
            (req "a" int53)
            (req "m" bool)))

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
  let to_string = Fmt.to_to_string pp
end

module Depth = struct
  module T = struct
    type t = {
      event_ts : Ptime.t ;
      symbol : string ;
      first_update_id : int64 ;
      last_update_id : int64 ;
      bids : Level.t list ;
      asks : Level.t list ;
    } [@@deriving sexp]

    let compare a b = Int64.compare a.last_update_id b.last_update_id
  end

  include T
  module Set = Set.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; symbol ; first_update_id ;
             last_update_id ; bids ; asks } ->
        ((), (event_ts, symbol, first_update_id, last_update_id, bids, asks)))
      (fun ((), (event_ts, symbol, first_update_id, last_update_id, bids, asks)) ->
         { event_ts ; symbol ; first_update_id ;
           last_update_id ; bids ; asks })
      (merge_objs unit (obj6
                          (req "E" Ptime.encoding)
                          (req "s" string)
                          (req "U" int53)
                          (req "u" int53)
                          (req "b" (list Level.encoding))
                          (req "a" (list Level.encoding))))
  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
end

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

module Stream = struct
  type t = {
    topic : topic ;
    symbol : string ;
  }

  let create ~topic ~symbol =
    { topic ; symbol = String.lowercase_ascii symbol }

  let pp ppf { topic ; symbol } =
    Format.pp_print_string ppf symbol ;
    Format.pp_print_char ppf '@' ;
    pp_topic ppf topic

  let to_string = Fmt.to_to_string pp
  let of_string s =
    match String.split_on_char '@' s with
    | [symbol; topic] -> { topic = topic_of_string topic ; symbol }
    | _ -> invalid_arg "stream_of_string"

  let to_path streams =
    String.concat "/" (List.map to_string streams)
end

type t =
  | Trade of Trade.t
  | Depth of Depth.t
[@@deriving sexp]

let pp ppf e =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t e)

let trade t = Trade t
let depth d = Depth d

let encoding =
  let open Json_encoding in
  union [
    case Trade.encoding (function Trade t -> Some t | _ -> None) trade ;
    case Depth.encoding (function Depth d -> Some d | _ -> None) depth ;
  ]

let encoding =
  let open Json_encoding in
  let to_json = function
      | Trade t -> (t.symbol ^ "@trade", Trade t)
      | Depth d -> (d.symbol ^ "@depth", Depth d) in
  let of_json (_, data) = data in
  conv to_json of_json
    (obj2
       (req "stream" string)
       (req "data" encoding))

