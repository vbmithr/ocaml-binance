open Core
open Async

open Binance_ws

let src = Logs.Src.create "binance.ws.console"

let main streams =
  let streams = List.map ~f:Stream.of_string streams in
  let buf = Bi_outbuf.create 4096 in
  Fastws_async.with_connection
    ~of_string:(Binance_ws_async.of_string ~buf)
    ~to_string:(fun _ -> assert false)
    (Binance_ws.url streams) ~f:begin fun _ r w ->
    Pipe.close w ;
    Pipe.iter r ~f:begin function
      | Trade t -> Logs_async.app ~src (fun m -> m "%a" Trade.pp t)
      | Depth d -> Logs_async.app ~src (fun m -> m "%a" Depth.pp d)
    end
  end

let command =
  Command.async ~summary:"Binance terminal" begin
    let open Command.Let_syntax in
    [%map_open
      let streams = anon (sequence ("stream" %: string))
      and () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        Deferred.Or_error.ok_exn (main streams)
    ] end

let () = Command.run command
