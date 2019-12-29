val of_string : ?buf:Bi_outbuf.t -> string -> Binance_ws.t

module Persistent : Persistent_connection_kernel.S
  with type address = Uri.t
   and type conn = (Binance_ws.t, unit) Fastws_async.t
