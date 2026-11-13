open Obook
open Fixed.O

let eps = 1e-6

let check msg cond =
  if not cond then (
    Printf.eprintf "[FAIL] %s\n%!" msg;
    exit 1
  )

let check_feq msg a b =
  if Float.abs (a -. b) > eps then (
    Printf.eprintf "[FAIL] %s: got %.10f, expected %.10f\n%!" msg a b;
    exit 1
  )

let check_audit msg book =
  match Book.audit book with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "[AUDIT FAIL] %s: %s\n%!" msg e;
    exit 1

let test_fixed () =
  let a = Fixed.of_str_exn "100000.00000000" in
  let b = Fixed.of_str_exn "1000.00000000" in
  let prod = a *~ b in
  check "128-bit mul $100k * 1000" (Fixed.to_str ~dec:2 prod = "100000000.00");

  let quot = prod /~ a in
  check "128-bit div $100M / $100k" (quot =~ b);

  let one_tick = Fixed.of_raw 1L in
  let two_ticks = Fixed.of_raw 2L in
  let three_ticks = Fixed.of_raw 3L in
  let two = Fixed.of_int_exn 2 in
  let r1 = Fixed.div_exn ~round:Half_even one_tick two in
  let r2 = Fixed.div_exn ~round:Half_even three_ticks two in
  check "Banker's rounding 1/2 -> 0 (even)" (r1 =~ Fixed.zero);
  check "Banker's rounding 3/2 -> 2 (even)" (r2 =~ two_ticks);

  let r_up = Fixed.div_exn ~round:Up one_tick two in
  let r_down = Fixed.div_exn ~round:Down (Fixed.of_raw (-1L)) two in
  check "Round Up +0.5 tick -> 1 tick" (r_up =~ one_tick);
  check "Round Down -0.5 tick -> -1 tick" (r_down =~ Fixed.of_raw (-1L));

  check "max_val + 1 overflows" (Fixed.add Fixed.max_val one_tick = Error `Overflow);
  check "min_val - 1 overflows" (Fixed.sub Fixed.min_val one_tick = Error `Overflow);
  check "neg min_val overflows" (Fixed.neg Fixed.min_val = Error `Overflow);
  check "div by zero trapped" (Fixed.div a Fixed.zero = Error `Div_by_zero);

  let bid_px = Fixed.of_str_exn "95000.00" in
  let ask_px = Fixed.of_str_exn "95010.00" in
  let bid_sz = Fixed.of_str_exn "3.0" in
  let ask_sz = Fixed.of_str_exn "1.0" in
  match Fixed.wmid ~bid_px ~bid_sz ~ask_px ~ask_sz with
  | Error _ -> check "wmid ok" false
  | Ok mp -> check_feq "wmid 3:1 skews toward ask (95007.5)" (Fixed.to_f mp) 95007.5

let test_stats () =
  let w1 = Stats.Welford.create () in
  let w2 = Stats.Welford.create () in
  let base = 1_000_000_000.0 in
  List.iter (Stats.Welford.push w1) [ base +. 1.0; base +. 2.0; base +. 3.0 ];
  List.iter (Stats.Welford.push w2) [ base +. 4.0; base +. 5.0 ];
  let w_all = Stats.Welford.merge w1 w2 in
  check "Welford n = 5" (Stats.Welford.n w_all = 5);
  (match Stats.Welford.var w_all with
   | None -> check "Welford var exists" false
   | Some v -> check_feq "Welford variance of [1..5] + 10^9 is 2.5" v 2.5);

  let ew = Stats.Ewma.of_span 10 in
  check "EWMA empty is None" (Stats.Ewma.mean ew = None);
  Stats.Ewma.push ew 100.0;
  check "EWMA n=1 var is None" (Stats.Ewma.var ew = None);
  Stats.Ewma.push ew 110.0;
  (match Stats.Ewma.vol ew with
   | None -> check "EWMA vol at n=2 exists" false
   | Some v -> check "EWMA vol > 0" (v > 0.0));

  let ofi = Stats.Ofi.create () in
  let p100 = Fixed.of_int_exn 100 and p101 = Fixed.of_int_exn 101 in
  let q10 = Fixed.of_int_exn 10 and q15 = Fixed.of_int_exn 15 in
  let _ = Stats.Ofi.push_bbo ofi ~bid_px:p100 ~bid_sz:q10 ~ask_px:p101 ~ask_sz:q10 in
  let d1 = Stats.Ofi.push_bbo ofi ~bid_px:p100 ~bid_sz:q15 ~ask_px:p101 ~ask_sz:q10 in
  check_feq "OFI bid addition +5" d1 5.0;

  let vpin = Stats.Vpin.create ~bucket_vol:(Fixed.of_int_exn 100) ~n_buckets:4 in
  Stats.Vpin.push_trade vpin ~is_buy:true ~qty:(Fixed.of_int_exn 250);
  check "VPIN 2 buckets completed" (Stats.Vpin.completed_buckets vpin = 2);
  check "VPIN not yet warmed up (2 < 4)" (not (Stats.Vpin.warmed_up vpin));
  (match Stats.Vpin.get vpin with
   | None -> check "VPIN partial value exists" false
   | Some v -> check_feq "Pure buy flow VPIN = 1.0" v 1.0);
  Stats.Vpin.push_trade vpin ~is_buy:false ~qty:(Fixed.of_int_exn 50);
  Stats.Vpin.push_trade vpin ~is_buy:true ~qty:(Fixed.of_int_exn 50);
  Stats.Vpin.push_trade vpin ~is_buy:false ~qty:(Fixed.of_int_exn 50);
  check "VPIN warmed up (4 buckets)" (Stats.Vpin.warmed_up vpin);
  (match Stats.Vpin.get vpin with
   | None -> check "VPIN value exists" false
   | Some v -> check_feq "2 pure buy buckets + 2 balanced buckets -> VPIN = 0.5" v 0.5);

  let roll = Stats.Roll.create ~alpha:0.01 () in
  let debruijn = [| 100.5; 100.5; 100.5; 99.5; 100.5; 99.5; 99.5; 99.5 |] in
  for i = 0 to 1599 do
    Stats.Roll.push_px roll debruijn.(i mod 8)
  done;
  match Stats.Roll.spread roll with
  | None -> check "Roll spread exists" false
  | Some sp -> check_feq "Roll spread on De Bruijn bounce ~1.0" (Float.round (sp *. 10.0) /. 10.0) 1.0

let test_book_mechanics () =
  let b = Book.create ~stp:Cancel_newest ~symbol:"BTC-USD" () in
  let px p = Fixed.of_int_exn p in
  let qty q = Fixed.of_int_exn q in

  (* 1. Iceberg FIFO tail replenishment *)
  (* Order 1: Iceberg Ask at 100, total 30, peak 10 (disp=10, hid=20) *)
  let r1 =
    Book.add_limit b ~id:1 ~trader:10 ~side:Ask ~px:(px 100) ~qty:(qty 30) ~iceberg_peak:(qty 10) ()
  in
  check "r1 conservation" (Book.verify_report_conservation r1);
  (* Order 2: Normal Ask at 100, qty 10 *)
  let r2 = Book.add_limit b ~id:2 ~trader:20 ~side:Ask ~px:(px 100) ~qty:(qty 10) () in
  check "r2 conservation" (Book.verify_report_conservation r2);
  check_audit "after iceberg + normal ask" b;

  (* Aggressive Buy for 15 @ 100:
     Must fill Order 1's first 10 (sending Order 1's replenished 10 to back of queue),
     and then fill 5 from Order 2! *)
  let r3 = Book.add_limit b ~id:3 ~trader:30 ~side:Bid ~px:(px 100) ~qty:(qty 15) () in
  check "r3 conservation" (Book.verify_report_conservation r3);
  check "r3 filled 15" (r3.filled_qty =~ qty 15);
  check "r3 has 2 fills (Order 1 for 10, Order 2 for 5)" (List.length r3.fills = 2);
  let f_first = List.hd r3.fills in
  let f_second = List.nth r3.fills 1 in
  check "First fill is Order 1 (10)" (f_first.maker_id = 1 && f_first.qty =~ qty 10);
  check "Second fill is Order 2 (5)" (f_second.maker_id = 2 && f_second.qty =~ qty 5);
  check_audit "after iceberg replenishment" b;

  (* Verify Order 2 is now ahead of replenished Order 1 in the queue! *)
  let q_100 = Book.l3_orders b ~side:Ask ~px:(px 100) in
  check "Order 2 is now head of queue" ((List.hd q_100).id = 2);

  (* 2. FOK read-only rejection (available at 100 is 5 + 20 = 25; requesting 30 must reject without mutating) *)
  let r_fok =
    Book.add_limit b ~id:4 ~trader:40 ~side:Bid ~px:(px 100) ~qty:(qty 30) ~tif:FOK ()
  in
  check "FOK rejected" (r_fok.status = Rejected_fok);
  check "FOK conservation" (Book.verify_report_conservation r_fok);
  check "Book unchanged after rejected FOK" (List.length (Book.l3_orders b ~side:Ask ~px:(px 100)) = 2);
  check_audit "after FOK reject" b;

  (* 3. Post-Only rejection when crossing *)
  let r_po =
    Book.add_limit b ~id:5 ~trader:40 ~side:Bid ~px:(px 100) ~qty:(qty 5) ~tif:Post_only ()
  in
  check "Post-Only rejected on cross" (r_po.status = Rejected_post_only);
  check "Post-Only conservation" (Book.verify_report_conservation r_po);

  (* 4. Amend size down preserves queue priority; amend size up loses priority *)
  let _ = Book.add_limit b ~id:10 ~trader:50 ~side:Bid ~px:(px 99) ~qty:(qty 20) () in
  let _ = Book.add_limit b ~id:11 ~trader:51 ~side:Bid ~px:(px 99) ~qty:(qty 20) () in
  (match Book.amend b ~id:10 ~new_px:(px 99) ~new_qty:(qty 12) () with
   | Error _ -> check "amend down ok" false
   | Ok rep ->
     check "amend down conservation" (Book.verify_report_conservation rep);
     let q99 = Book.l3_orders b ~side:Bid ~px:(px 99) in
     check "Order 10 kept head priority after size reduction" ((List.hd q99).id = 10));
  (match Book.amend b ~id:10 ~new_px:(px 99) ~new_qty:(qty 25) () with
   | Error _ -> check "amend up ok" false
   | Ok rep ->
     check "amend up conservation" (Book.verify_report_conservation rep);
     let q99 = Book.l3_orders b ~side:Bid ~px:(px 99) in
     check "Order 10 moved to tail after size increase" ((List.hd q99).id = 11));
  check_audit "after amends" b;

  (* 5. Self-Trade Prevention (Cancel_newest) *)
  (* Order 11 at Bid 99 belongs to trader 51; Trader 51 sends Ask @ 99 *)
  let r_stp =
    Book.add_limit b ~id:12 ~trader:51 ~side:Ask ~px:(px 99) ~qty:(qty 5) ()
  in
  check "STP Cancel_newest triggered" (r_stp.status = Cancelled_stp);
  check "STP zero wash fills" (r_stp.filled_qty =~ Fixed.zero);
  check "STP conservation" (Book.verify_report_conservation r_stp);
  check_audit "after STP" b

let test_fuzz_50k () =
  let b = Book.create ~capacity:8192 ~stp:Decrement_and_cancel ~symbol:"ETH-USD" () in
  let rng = ref 133742069 in
  let next_rand () =
    rng := (!rng * 1103515245 + 12345) land 0x3fffffff;
    !rng
  in
  let live_ids = Array.make 4096 0 in
  let live_len = ref 0 in
  let remember_id id =
    if !live_len < 4096 then (
      live_ids.(!live_len) <- id;
      incr live_len
    )
  in
  for step = 1 to 50_000 do
    let action = next_rand () mod 100 in
    if action < 65 || !live_len = 0 then (
      let side : Book.side = if next_rand () mod 2 = 0 then Bid else Ask in
      let px = Fixed.of_int_exn (980 + (next_rand () mod 40)) in
      let qty = Fixed.of_int_exn (1 + (next_rand () mod 50)) in
      let trader = 1 + (next_rand () mod 12) in
      let tif : Book.tif =
        match next_rand () mod 10 with
        | 0 -> IOC
        | 1 -> FOK
        | 2 -> Post_only
        | _ -> GTC
      in
      let iceberg_peak =
        if next_rand () mod 6 = 0 then Some (Fixed.of_int_exn (1 + (next_rand () mod 10)))
        else None
      in
      let rep = Book.add_limit b ~id:step ~trader ~side ~px ~qty ~tif ?iceberg_peak () in
      if not (Book.verify_report_conservation rep) then (
        Printf.eprintf "[FUZZ FAIL] Conservation violated at step %d\n%!" step;
        exit 1
      );
      if rep.resting_qty >~ Fixed.zero then remember_id step
    ) else if action < 85 then (
      let idx = next_rand () mod !live_len in
      let id = live_ids.(idx) in
      live_ids.(idx) <- live_ids.(!live_len - 1);
      decr live_len;
      let _ = Book.cancel b ~id in
      ()
    ) else (
      let idx = next_rand () mod !live_len in
      let id = live_ids.(idx) in
      let new_px = Fixed.of_int_exn (980 + (next_rand () mod 40)) in
      let new_qty = Fixed.of_int_exn (1 + (next_rand () mod 50)) in
      match Book.amend b ~id ~new_px ~new_qty () with
      | Error `Not_found -> ()
      | Ok rep ->
        if not (Book.verify_report_conservation rep) then (
          Printf.eprintf "[FUZZ FAIL] Amend conservation violated at step %d\n%!" step;
          exit 1
        )
    );
    if step mod 2_500 = 0 then check_audit (Printf.sprintf "fuzz step %d" step) b
  done;
  check_audit "fuzz final" b

let test_itch_and_ez () =
  let open Itch in
  let msgs =
    [
      System_event { ts = 100L; code = 'O' };
      Add_order
        {
          ts = 101L;
          oid = 1001;
          side = Bid;
          qty = Fixed.of_int_exn 50;
          symbol = "AAPL";
          px = Fixed.of_str_exn "185.2500";
        };
      Add_order
        {
          ts = 102L;
          oid = 1002;
          side = Ask;
          qty = Fixed.of_int_exn 40;
          symbol = "AAPL";
          px = Fixed.of_str_exn "185.5000";
        };
      Order_executed { ts = 103L; oid = 1001; exec_qty = Fixed.of_int_exn 20 };
      Order_cancel { ts = 104L; oid = 1002; canc_qty = Fixed.of_int_exn 10 };
      Order_replace
        {
          ts = 105L;
          orig_oid = 1002;
          new_oid = 1003;
          new_qty = Fixed.of_int_exn 25;
          new_px = Fixed.of_str_exn "185.4000";
        };
      Order_delete { ts = 106L; oid = 1001 };
    ]
  in
  let wire = encode_all msgs in
  (match decode_all wire with
   | Error e ->
     Printf.eprintf "[FAIL] ITCH decode_all: %s\n%!" e;
     exit 1
   | Ok decoded ->
     check "ITCH roundtrip count = 7" (List.length decoded = 7);
     let bk = Book.create ~symbol:"AAPL" () in
     List.iter (Itch.apply bk) decoded;
     check_audit "after ITCH stream apply" bk;
     check "Only replaced Ask 1003 remains" (Book.order_count bk = 1);
     match Book.best_ask bk with
     | None -> check "best_ask exists" false
     | Some (ap, aq) ->
       check_feq "ITCH replaced ask px 185.40" (Fixed.to_f ap) 185.40;
       check_feq "ITCH replaced ask qty 25" (Fixed.to_f aq) 25.0);

  (* Ergonomic Ez interface check *)
  let ez = Obook.book "SOL-USD" in
  let bid_id, _ = Obook.bid ez ~px:150.0 ~qty:10.0 () in
  let ask_id, _ = Obook.ask ez ~px:150.5 ~qty:20.0 () in
  let mkt_rep = Obook.buy_mkt ez ~qty:5.0 () in
  check "Ez market buy filled 5.0" (Fixed.to_f mkt_rep.filled_qty = 5.0);
  check "Ez amend ask works" (Obook.amend ez ~id:ask_id ~px:150.5 ~qty:12.0);
  check "Ez cancel bid works" (Obook.cancel ez bid_id);
  let s = Obook.snap ez in
  check "Ez active orders = 1" (s.active_orders = 1);
  let json = Ez.to_json ez in
  check "Ez JSON non-empty" (String.length json > 40);
  let ascii = Ez.render_ascii ez in
  check "Ez ASCII table non-empty" (String.length ascii > 100)

let () =
  test_fixed ();
  test_stats ();
  test_book_mechanics ();
  test_fuzz_50k ();
  test_itch_and_ez ();
  Printf.printf "[OK] Iteration 3 (ITCH 5.0 Codec, Ez Ergonomic API, Book & 50k Fuzzer) all invariants verified.\n%!"
