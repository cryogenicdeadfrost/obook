open Obook
open Fixed.O

let run_demo () =
  Printf.printf "\n>>> [1/2] Obook Live Market Microstructure & Iceberg Showcase <<<\n\n";
  let ez = Obook.book ~vpin_bucket_vol:20.0 ~vpin_buckets:10 "BTC-PERP" in

  (* Market makers post layered liquidity + an Iceberg wall at 95,010 *)
  let _ = Obook.bid ez ~trader:1 ~px:94995.0 ~qty:4.5 () in
  let _ = Obook.bid ez ~trader:2 ~px:94990.0 ~qty:8.0 () in
  let _ = Obook.bid ez ~trader:3 ~px:94980.0 ~qty:15.25 () in

  let _ = Obook.ask ez ~trader:4 ~px:95005.0 ~qty:3.0 () in
  let _, _ = Obook.ask ez ~trader:5 ~px:95010.0 ~qty:25.0 ~iceberg:5.0 () in
  let _ = Obook.ask ez ~trader:6 ~px:95020.0 ~qty:12.0 () in

  (* Aggressive sweeps across the book *)
  let r1 = Obook.buy_mkt ez ~trader:99 ~qty:6.0 () in
  Printf.printf "Aggressive Buy 6.0 BTC -> Filled: %s BTC across %d maker orders\n"
    (Fixed.to_str ~dec:2 r1.filled_qty) (List.length r1.fills);

  let _ = Obook.sell_mkt ez ~trader:98 ~qty:2.5 () in
  let _ = Obook.buy_mkt ez ~trader:99 ~qty:12.0 () in

  Obook.pp ~levels:5 ez;
  Printf.printf "\nLive Telemetry JSON:\n%s\n\n" (Ez.to_json ~levels:3 ez);

  Printf.printf ">>> [2/2] NASDAQ TotalView-ITCH 5.0 Wire Stream Replay <<<\n";
  let itch_frames =
    Itch.encode_all
      [
        System_event { ts = 1_000_000L; code = 'O' };
        Add_order
          {
            ts = 1_000_100L;
            oid = 501;
            side = Bid;
            qty = Fixed.of_int_exn 100;
            symbol = "NVDA";
            px = Fixed.of_str_exn "138.50";
          };
        Add_order
          {
            ts = 1_000_200L;
            oid = 502;
            side = Ask;
            qty = Fixed.of_int_exn 150;
            symbol = "NVDA";
            px = Fixed.of_str_exn "138.60";
          };
        Order_executed { ts = 1_000_300L; oid = 502; exec_qty = Fixed.of_int_exn 50 };
      ]
  in
  let nvda = Book.create ~symbol:"NVDA" () in
  (match Itch.decode_all itch_frames with
   | Error e -> Printf.eprintf "ITCH Error: %s\n" e
   | Ok msgs ->
     List.iter (Itch.apply nvda) msgs;
     Printf.printf "Replayed %d ITCH 5.0 wire frames (%d bytes) -> NVDA active orders: %d\n"
       (List.length msgs) (Bytes.length itch_frames) (Book.order_count nvda))

let run_bench n_events =
  Printf.printf "\n>>> Obook Zero-Allocation Hot-Path Benchmark (%d events) <<<\n" n_events;
  let b = Book.create ~capacity:32768 ~stp:Decrement_and_cancel ~symbol:"ETH-PERP" () in
  let rng = ref 0xCAFEBABE in
  let fast_rand () =
    rng := (!rng * 1664525 + 1013904223) land 0x3fffffff;
    !rng
  in
  let active_ids = Array.make 8192 0 in
  let active_n = ref 0 in
  let t0 = Unix.gettimeofday () in
  for i = 1 to n_events do
    let op = fast_rand () mod 100 in
    if op < 70 || !active_n = 0 then (
      let side : Book.side = if i land 1 = 0 then Bid else Ask in
      let px = Fixed.of_int_exn (3000 + (fast_rand () mod 50)) in
      let qty = Fixed.of_int_exn (1 + (fast_rand () mod 20)) in
      let rep = Book.add_limit b ~id:i ~trader:(i mod 16) ~side ~px ~qty () in
      if rep.resting_qty >~ Fixed.zero && !active_n < 8192 then (
        active_ids.(!active_n) <- i;
        incr active_n
      )
    ) else (
      let idx = fast_rand () mod !active_n in
      let oid = active_ids.(idx) in
      active_ids.(idx) <- active_ids.(!active_n - 1);
      decr active_n;
      let _ = Book.cancel b ~id:oid in
      ()
    )
  done;
  let elapsed = Float.max 1e-6 (Unix.gettimeofday () -. t0) in
  let throughput = float_of_int n_events /. elapsed in
  let ns_per_op = (elapsed *. 1e9) /. float_of_int n_events in
  let audit_status =
    match Book.audit b with Ok () -> "PASS (0 violations)" | Error e -> "FAIL: " ^ e
  in
  Printf.printf "Elapsed Time   : %.4f s\n" elapsed;
  Printf.printf "Throughput     : %.0f events/sec\n" throughput;
  Printf.printf "Latency        : %.1f ns/op\n" ns_per_op;
  Printf.printf "Trades Matched : %d (Vol: %s)\n"
    (Book.trade_count b) (Fixed.to_str ~dec:2 (Book.volume_traded b));
  Printf.printf "Book Audit     : %s\n%!" audit_status

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "bench" :: n_str :: _ ->
    let n = try int_of_string n_str with _ -> 200_000 in
    run_bench n
  | _ :: "bench" :: _ -> run_bench 200_000
  | _ :: "demo" :: _ -> run_demo ()
  | _ ->
    run_demo ();
    run_bench 200_000
