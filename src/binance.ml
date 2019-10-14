open Sexplib.Std

module Side = struct
  type t = Fixtypes.Side.t = Buy | Sell [@@deriving sexp]

  let pp ppf = function
    | Fixtypes.Side.Buy -> Format.pp_print_string ppf "BUY"
    | Sell -> Format.pp_print_string ppf "SELL"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "BUY" -> Some Fixtypes.Side.Buy
    | "SELL" -> Some Sell
    | _ -> None

  let of_string_exn = function
    | "BUY" -> Fixtypes.Side.Buy
    | "SELL" -> Sell
    | _ -> invalid_arg "Side.of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

module TimeInForce = struct
  type t =
    | GoodTillCanceled
    | ImmediateOrCancel
    | FillOrKill

  let pp ppf = function
    | GoodTillCanceled  -> Format.pp_print_string ppf "GTC"
    | ImmediateOrCancel -> Format.pp_print_string ppf "IOC"
    | FillOrKill        -> Format.pp_print_string ppf "FOK"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "GTC" -> Some GoodTillCanceled
    | "IOC" -> Some ImmediateOrCancel
    | "FOK" -> Some FillOrKill
    | _     -> None

  let of_string_exn = function
    | "GTC" -> GoodTillCanceled
    | "IOC" -> ImmediateOrCancel
    | "FOK" -> FillOrKill
    | _     -> invalid_arg "TimeInForce.of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

module OrderStatus = struct
  type t =
    | New
    | Partially_filled
    | Filled
    | Canceled
    | Pending_cancel
    | Rejected
    | Expired

  let pp ppf = function
    | New              -> Format.pp_print_string ppf "NEW"
    | Partially_filled -> Format.pp_print_string ppf "PARTIALLY_FILLED"
    | Filled           -> Format.pp_print_string ppf "FILLED"
    | Canceled         -> Format.pp_print_string ppf "CANCELED"
    | Pending_cancel   -> Format.pp_print_string ppf "PENDING_CANCEL"
    | Rejected         -> Format.pp_print_string ppf "REJECTED"
    | Expired          -> Format.pp_print_string ppf "EXPIRED"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "NEW"              -> Some New
    | "PARTIALLY_FILLED" -> Some Partially_filled
    | "FILLED"           -> Some Filled
    | "CANCELED"         -> Some Canceled
    | "PENDING_CANCEL"   -> Some Pending_cancel
    | "REJECTED"         -> Some Rejected
    | "EXPIRED"          -> Some Rejected
    | _                  -> None

  let of_string_exn = function
    | "NEW"              -> New
    | "PARTIALLY_FILLED" -> Partially_filled
    | "FILLED"           -> Filled
    | "CANCELED"         -> Canceled
    | "PENDING_CANCEL"   -> Pending_cancel
    | "REJECTED"         -> Rejected
    | "EXPIRED"          -> Rejected
    | _                  -> invalid_arg "OrderStatus.of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

module OrderType = struct
  type t =
    | Limit
    | Market
    | Stop_loss
    | Stop_loss_limit
    | Take_profit
    | Take_profit_limit
    | Limit_maker

  let pp ppf = function
    | Limit             -> Format.pp_print_string ppf "LIMIT"
    | Market            -> Format.pp_print_string ppf "MARKET"
    | Stop_loss         -> Format.pp_print_string ppf "STOP_LOSS"
    | Stop_loss_limit   -> Format.pp_print_string ppf "STOP_LOSS_LIMIT"
    | Take_profit       -> Format.pp_print_string ppf "TAKE_PROFIT"
    | Take_profit_limit -> Format.pp_print_string ppf "TAKE_PROFIT_LIMIT"
    | Limit_maker       -> Format.pp_print_string ppf "LIMIT_MAKER"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "LIMIT"             -> Some Limit
    | "MARKET"            -> Some Market
    | "STOP_LOSS"         -> Some Stop_loss
    | "STOP_LOSS_LIMIT"   -> Some Stop_loss_limit
    | "TAKE_PROFIT"       -> Some Take_profit
    | "TAKE_PROFIT_LIMIT" -> Some Take_profit_limit
    | "LIMIT_MAKER"       -> Some Limit_maker
    | _                   -> None

  let of_string_exn = function
    | "LIMIT"             -> Limit
    | "MARKET"            -> Market
    | "STOP_LOSS"         -> Stop_loss
    | "STOP_LOSS_LIMIT"   -> Stop_loss_limit
    | "TAKE_PROFIT"       -> Take_profit
    | "TAKE_PROFIT_LIMIT" -> Take_profit_limit
    | "LIMIT_MAKER"       -> Limit_maker
    | _                   -> invalid_arg "OrderType.of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

module Yojson_repr = struct
  include Json_encoding.Make(Json_repr.Yojson)
  let destruct_safe encoding v =
    try destruct encoding v with exn ->
      Format.eprintf "%a@."
        (fun ppf exn -> Json_encoding.print_error ppf exn) exn ;
      raise exn
end

let safe_float =
  let open Json_encoding in
  union [
    case float (fun a -> Some a) (fun a -> a) ;
    case string (fun a -> Some (Float.to_string a)) Float.of_string
  ]

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Json_encoding in
    conv
      (fun i -> Int64.of_float (Ptime.to_float_s i *. 1e3))
      (fun ts -> match Ptime.of_float_s (Int64.to_float ts /. 1e3) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      int53
end

module Level = struct
  type t = {
    p : float ;
    q : float ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { p ; q } -> (Float.to_string p, Float.to_string q))
      (fun (p, q) -> { p = Float.of_string p ; q = Float.of_string q })
      (tup2 string string)
end

module Sym = struct
  type t = {
    symbol: string ;
    base: string ;
    base_decimals: int ;
    quote: string ;
    quote_decimals: int ;
  } [@@deriving sexp]

  let compare a b = String.compare a.symbol b.symbol

  let encoding =
    let open Json_encoding in
    conv
      (fun { symbol ; base ; base_decimals ; quote ; quote_decimals } ->
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
end
