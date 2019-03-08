open Core
open Async
open Binance

let src = Logs.Src.create "binance.test.depth"

let drop_events_before depth last_update_id =
  let _before, after =
    Set.partition_tf depth ~f:(fun { Depth.final_update_id ; _ } ->
        final_update_id <= last_update_id) in
  after

let merge_diffs b a { Depth.bids ; Depth.asks ; _ } =
  let b = List.fold_left bids ~init:b ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  let a = List.fold_left asks ~init:a ~f:begin fun acc { p ; q } ->
      Map.(if q = 0. then remove acc p else set acc ~key:p ~data:q)
    end in
  b, a

let orderbook symbol init c_write =
  let evts = Ws.connect
      Ws.[create_stream ~topic:Depth ~symbol] in
  Pipe.fold evts
    ~init:(
      None, (* previous event *)
      Set.empty (module Depth), (* events unprocessed *)
      Map.empty (module Float), (* bids *)
      Map.empty (module Float)) (* asks *)
    ~f:begin fun ((prev_d, s, b, a) as acc) -> function
    | Trade _ -> Deferred.return acc
    | Depth d ->
      match Ivar.peek init with
      | None ->
        (* store events *)
        Pipe.write c_write (Some d, None, None) >>= fun () ->
        return (Some d, (Set.add s d), b, a)
      | Some (last_update_id, _, _)
        when (not (Map.is_empty b) || not (Map.is_empty b)) ->
        (* already inited, add event if compliant *)
        let last_update_id =
          Option.value_map prev_d ~default:last_update_id
            ~f:(fun { final_update_id ; _ } -> final_update_id) in
        if d.Depth.first_update_id <> last_update_id + 1 then
          failwith "orderbook: sequence problem, aborting" ;
        let b, a = merge_diffs b a d in
        Pipe.write c_write (Some d, Some b, Some a) >>= fun () ->
        return (Some d, s, b, a)
      | Some (last_update_id, bids, asks) -> begin
          (* initialization phase *)
          let evts = drop_events_before (Set.add s d) last_update_id in
          match Set.min_elt evts with
          | None ->
            (* No previous events received *)
            Pipe.write c_write (None, Some bids, Some asks) >>= fun () ->
            return (None, s, bids, asks)
          | Some { first_update_id ; final_update_id ; _ } ->
            (* Previous events received *)
            if first_update_id > last_update_id + 1 ||
               final_update_id < last_update_id + 1 then
              failwithf "orderbook: inconsistent data received (%d %d %d)"
                first_update_id final_update_id last_update_id () ;
            let prev_d, bids, asks =
              Set.fold evts
                ~init:(None, bids, asks)
                ~f:begin fun (_prev_d, bids, asks) d ->
                let bids, asks = merge_diffs bids asks d in
                Some d, bids, asks
              end in
            Pipe.write c_write (None, Some bids, Some asks) >>= fun () ->
            Deferred.return (prev_d, s, bids, asks)
        end
    end

let load_books b a =
  let b = List.fold_left b
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc ~key:p ~data:q
    end in
  let a = List.fold_left a
      ~init:(Map.empty (module Float)) ~f:begin fun acc { Level.p ; q } ->
      Map.set acc ~key:p ~data:q
    end in
  b, a

let init_orderbook symbol =
  Rest.Depth.get ~limit:100 symbol >>|
  Result.map ~f:begin fun { Rest.Depth.last_update_id ; bids ; asks } ->
    let bids, asks = load_books bids asks in
    last_update_id, bids, asks
  end

let wait_n_events c_read n =
  let rec inner n =
    Logs_async.app ~src (fun m -> m "wait for %d events" n) >>= fun () ->
    if n > 0 then
      Pipe.read c_read >>= fun _ ->
      inner (pred n)
    else
      Deferred.unit
  in inner n

let main symbol =
  let init = Ivar.create () in
  let c_read, c_write = Pipe.create () in
  don't_wait_for (Deferred.ignore (orderbook symbol init c_write)) ;
  wait_n_events c_read 10 >>= fun () ->
  begin
    init_orderbook symbol >>= function
    | Error err ->
      Logs_async.app ~src (fun m -> m "%a" Rest.BinanceError.pp err) >>= fun () ->
      failwith "Init orderbook failed"
    | Ok snapshot ->
      Logs_async.app ~src (fun m -> m "Got snapshot for %s" symbol) >>= fun () ->
      Ivar.fill init snapshot ;
      Pipe.iter c_read ~f:begin fun (d, _bids, _asks) ->
        match d with
        | None ->
          Logs_async.app ~src (fun m -> m "Order books initialized %s" symbol)
        | Some _ ->
          Logs_async.app ~src (fun m -> m "Order books updated")
      end
  end

let command =
  Command.async ~summary:"Binance depth" begin
    let open Command.Let_syntax in
    [%map_open
      let symbol = anon ("symbol" %: string)
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main symbol
    ] end

let () = Command.run command
