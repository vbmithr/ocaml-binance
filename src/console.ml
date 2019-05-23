open Core
open Async
open Binance

open Binance_ws

let src = Logs.Src.create "binance.ws.console"

let command =
  Command.async ~summary:"Binance terminal" begin
    let open Command.Let_syntax in
    [%map_open
      let streams = anon (sequence ("stream" %: string))
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        Binance_ws_async.connect
          (List.map ~f:Stream.of_string streams) >>= fun (evts, _cleaned_up) ->
        Pipe.iter evts ~f:begin function
          | Trade t ->
            Logs_async.app ~src (fun m -> m "T %a" Trade.pp t)
          | Depth d ->
            Logs_async.app ~src (fun m -> m "D %a" Depth.pp d)
        end
    ] end

let () = Command.run command
