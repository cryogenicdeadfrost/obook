(* Market Maker Toxic Flow Detection:
   Simulates balanced retail flow followed by an informed institutional whale sweep,
   demonstrating how VPIN and Cont's OFI detect toxicity in O(1) time. *)

let () =
  let b = Obook.book ~vpin_bucket_vol:50.0 ~vpin_buckets:10 "ETH-USD" in

  (* Seed deep order book *)
  for i = 1 to 20 do
    let fi = float_of_int i in
    let _ = Obook.bid b ~px:(3000.0 -. fi) ~qty:25.0 () in
    let _ = Obook.ask b ~px:(3000.0 +. fi) ~qty:25.0 () in
    ()
  done;

  Printf.printf "--- Phase 1: Balanced Uninformed Retail Flow ---\n";
  for i = 1 to 10 do
    if i mod 2 = 0 then ignore (Obook.buy_mkt b ~qty:25.0 ())
    else ignore (Obook.sell_mkt b ~qty:25.0 ());
    let _ = Obook.bid b ~px:2999.0 ~qty:25.0 () in
    let _ = Obook.ask b ~px:3001.0 ~qty:25.0 () in
    ()
  done;
  let s1 = Obook.snap b in
  Printf.printf "Balanced Regime -> VPIN: %.3f | Decayed OFI: %+.2f\n\n"
    (Option.value ~default:0.0 s1.vpin) s1.ofi;

  Printf.printf "--- Phase 2: Informed Whale Directional Sweep ---\n";
  for _ = 1 to 8 do
    ignore (Obook.buy_mkt b ~qty:60.0 ())
  done;
  let s2 = Obook.snap b in
  Printf.printf "Toxic Sweep     -> VPIN: %.3f | Decayed OFI: %+.2f (ALERT: Widen Spreads!)\n"
    (Option.value ~default:0.0 s2.vpin) s2.ofi
