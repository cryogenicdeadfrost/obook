open Obook

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

let test_fixed () =
  let open Fixed.O in
  let a = Fixed.of_str_exn "100000.00000000" in
  let b = Fixed.of_str_exn "1000.00000000" in
  (* 100,000 * 1,000 = 100,000,000 -> raw product is 10^24, way past 2^63-1 *)
  let prod = a *~ b in
  check "128-bit mul $100k * 1000" (Fixed.to_str ~dec:2 prod = "100000000.00");

  let quot = prod /~ a in
  check "128-bit div $100M / $100k" (quot =~ b);

  (* Banker's rounding (Half_even) check: 0.5 rounds to even last tick *)
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

  (* Overflow traps at Int64 boundaries *)
  check "max_val + 1 overflows" (Fixed.add Fixed.max_val one_tick = Error `Overflow);
  check "min_val - 1 overflows" (Fixed.sub Fixed.min_val one_tick = Error `Overflow);
  check "neg min_val overflows" (Fixed.neg Fixed.min_val = Error `Overflow);
  check "div by zero trapped" (Fixed.div a Fixed.zero = Error `Div_by_zero);

  (* Volume-weighted micro-price *)
  let bid_px = Fixed.of_str_exn "95000.00" in
  let ask_px = Fixed.of_str_exn "95010.00" in
  let bid_sz = Fixed.of_str_exn "3.0" in
  let ask_sz = Fixed.of_str_exn "1.0" in
  match Fixed.wmid ~bid_px ~bid_sz ~ask_px ~ask_sz with
  | Error _ -> check "wmid ok" false
  | Ok mp -> check_feq "wmid 3:1 skews toward ask (95007.5)" (Fixed.to_f mp) 95007.5

let test_stats () =
  (* 1. Welford numerical stability at 10^9 offset *)
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

  (* 2. Finch EWMA *)
  let ew = Stats.Ewma.of_span 10 in
  check "EWMA empty is None" (Stats.Ewma.mean ew = None);
  Stats.Ewma.push ew 100.0;
  check "EWMA n=1 var is None" (Stats.Ewma.var ew = None);
  Stats.Ewma.push ew 110.0;
  (match Stats.Ewma.vol ew with
   | None -> check "EWMA vol at n=2 exists" false
   | Some v -> check "EWMA vol > 0" (v > 0.0));

  (* 3. Cont OFI *)
  let ofi = Stats.Ofi.create () in
  let p100 = Fixed.of_int_exn 100 and p101 = Fixed.of_int_exn 101 in
  let q10 = Fixed.of_int_exn 10 and q15 = Fixed.of_int_exn 15 in
  let _ = Stats.Ofi.push_bbo ofi ~bid_px:p100 ~bid_sz:q10 ~ask_px:p101 ~ask_sz:q10 in
  (* Bid size increases by +5 at same price, Ask size unchanged -> OFI = +5 *)
  let d1 = Stats.Ofi.push_bbo ofi ~bid_px:p100 ~bid_sz:q15 ~ask_px:p101 ~ask_sz:q10 in
  check_feq "OFI bid addition +5" d1 5.0;

  (* 4. VPIN multi-bucket whale slicing *)
  let vpin = Stats.Vpin.create ~bucket_vol:(Fixed.of_int_exn 100) ~n_buckets:4 in
  (* Single buy whale trade of 250 spans 2 full buckets + 50 into 3rd *)
  Stats.Vpin.push_trade vpin ~is_buy:true ~qty:(Fixed.of_int_exn 250);
  check "VPIN 2 buckets completed" (Stats.Vpin.completed_buckets vpin = 2);
  check "VPIN not yet warmed up (2 < 4)" (not (Stats.Vpin.warmed_up vpin));
  (match Stats.Vpin.get vpin with
   | None -> check "VPIN partial value exists" false
   | Some v -> check_feq "Pure buy flow VPIN = 1.0" v 1.0);
  (* Balanced flow in next buckets *)
  Stats.Vpin.push_trade vpin ~is_buy:false ~qty:(Fixed.of_int_exn 50);
  Stats.Vpin.push_trade vpin ~is_buy:true ~qty:(Fixed.of_int_exn 50);
  Stats.Vpin.push_trade vpin ~is_buy:false ~qty:(Fixed.of_int_exn 50);
  check "VPIN warmed up (4 buckets)" (Stats.Vpin.warmed_up vpin);
  (match Stats.Vpin.get vpin with
   | None -> check "VPIN value exists" false
   | Some v -> check_feq "2 pure buy buckets + 2 balanced buckets -> VPIN = 0.5" v 0.5);

  (* 5. Roll effective spread on bid-ask bounce using an order-3 De Bruijn cycle
     so all 8 triples (Q_{t-2}, Q_{t-1}, Q_t) occur with exact 1/8 frequency. *)
  let roll = Stats.Roll.create ~alpha:0.01 () in
  let debruijn = [| 100.5; 100.5; 100.5; 99.5; 100.5; 99.5; 99.5; 99.5 |] in
  for i = 0 to 1599 do
    Stats.Roll.push_px roll debruijn.(i mod 8)
  done;
  match Stats.Roll.spread roll with
  | None -> check "Roll spread exists" false
  | Some sp -> check_feq "Roll spread on De Bruijn bounce ~1.0" (Float.round (sp *. 10.0) /. 10.0) 1.0

let () =
  test_fixed ();
  test_stats ();
  Printf.printf "[OK] Iteration 1 (Fixed & Stats) all invariants verified.\n%!"
