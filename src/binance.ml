module Side = struct
  type t = [`Buy | `Sell]

  let pp ppf = function
    | `Buy -> Format.pp_print_string ppf "BUY"
    | `Sell -> Format.pp_print_string ppf "SELL"

  let to_string = Fmt.to_to_string pp

  let of_string = function
    | "BUY" -> Some `Buy
    | "SELL" -> Some `Sell
    | _ -> None

  let of_string_exn = function
    | "BUY" -> `Buy
    | "SELL" -> `Sell
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

open Base

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    Option.value_exn (Ptime.of_float_s (Sexplib.Std.float_of_sexp sexp))
  let sexp_of_t t =
    Sexplib.Std.sexp_of_float (Ptime.to_float_s t)

  let float_encoding =
    let open Json_encoding in
    conv
      (fun i -> Ptime.to_float_s i *. 1e3)
      (fun i -> Option.value ~default:Ptime.epoch (Ptime.of_float_s (i /. 1e3)))
      float
end
let float_as_string =
  let open Json_encoding in
  conv Float.to_string Float.of_string string

module Trade = struct
  module T = struct
    type t = {
      event_ts : Ptime.t ;
      trade_ts : Ptime.t ;
      symbol : string ;
      tid : int ;
      p : float ;
      q : float ;
      buyer_order_id : int ;
      seller_order_id : int ;
      buyer_is_mm : bool ;
    } [@@deriving sexp]

    let compare { trade_ts = a ; _ } { trade_ts = b ; _ } =
      Ptime.compare a b
  end
  include T
  include Comparable.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; trade_ts ; symbol ; tid ; p ; q ;
             buyer_order_id ; seller_order_id ; buyer_is_mm } ->
        (), (event_ts, trade_ts, symbol, tid, p, q,
             buyer_order_id, seller_order_id, buyer_is_mm))
      (fun ((), (event_ts, trade_ts, symbol, tid, p, q,
                 buyer_order_id, seller_order_id, buyer_is_mm)) ->
        { event_ts ; trade_ts ; symbol ; tid ; p ; q ;
          buyer_order_id ; seller_order_id ; buyer_is_mm })
      (merge_objs unit (obj9
                          (req "E" Ptime.float_encoding)
                          (req "T" Ptime.float_encoding)
                          (req "s" string)
                          (req "t" int)
                          (req "p" float_as_string)
                          (req "q" float_as_string)
                          (req "b" int)
                          (req "a" int)
                          (req "m" bool)))

  let pp ppf t =
    Json_repr.(pp (module Yojson) ppf (Yojson_repr.construct encoding t))
  let to_string = Fmt.to_to_string pp
end

module Level = struct
  module T = struct
    type t = {
      p : float ;
      q : float ;
    } [@@deriving sexp]

    let compare { p = a ; _ } { p = b ; _ } =
      Float.compare a b
  end
  include T
  include Comparable.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun { p ; q } -> (Float.to_string p, Float.to_string q))
      (fun (p, q) -> { p = Float.of_string p ; q = Float.of_string q })
      (tup2 string string)
end

module Depth = struct
  module T = struct
    type t = {
      event_ts : Ptime.t ;
      symbol : string ;
      first_update_id : int ;
      final_update_id : int ;
      bids : Level.t list ;
      asks : Level.t list ;
    } [@@deriving sexp]

    let compare { final_update_id = a ; _ } { final_update_id = b ; _ } =
      Int.compare a b
  end
  include T
  include Comparable.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun { event_ts ; symbol ; first_update_id ;
             final_update_id ; bids ; asks } ->
        ((), (event_ts, symbol, first_update_id, final_update_id, bids, asks)))
      (fun ((), (event_ts, symbol, first_update_id, final_update_id, bids, asks)) ->
         { event_ts ; symbol ; first_update_id ;
           final_update_id ; bids ; asks })
      (merge_objs unit (obj6
                          (req "E" Ptime.float_encoding)
                          (req "s" string)
                          (req "U" int)
                          (req "u" int)
                          (req "b" (list Level.encoding))
                          (req "a" (list Level.encoding))))
  let pp ppf t =
    Json_repr.(pp (module Yojson) ppf (Yojson_repr.construct encoding t))
  let to_string = Fmt.to_to_string pp
end
