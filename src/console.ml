open Core
open Async

open Binance_ws

let src = Logs.Src.create "binance.ws.console"

let main streams =
  Binance_ws_async.with_connection_exn
    (List.map ~f:Stream.of_string streams) ~f:begin fun evts ->
    Pipe.iter evts ~f:begin function
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
        main streams
    ] end

let () = Command.run command
