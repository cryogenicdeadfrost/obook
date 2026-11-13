(* 30-second Quickstart: Create an L3 book, post passive + iceberg liquidity,
   sweep with market orders, and inspect microstructure telemetry. *)

let () =
  let book = Obook.book "SOL-USD" in

  (* Post Bid & Ask liquidity *)
  let _bid1, _ = Obook.bid book ~px:180.25 ~qty:25.0 () in
  let _bid2, _ = Obook.bid book ~px:180.00 ~qty:50.0 () in
  let _ask1, _ = Obook.ask book ~px:180.50 ~qty:15.0 () in
  (* Iceberg Ask: 100 SOL total, only 10 SOL visible on L2 at a time *)
  let _ask_ice, _ = Obook.ask book ~px:180.75 ~qty:100.0 ~iceberg:10.0 () in

  (* Execute an aggressive market buy of 20 SOL *)
  let report = Obook.buy_mkt book ~qty:20.0 () in
  Printf.printf "Market Buy filled %.2f SOL across %d executions.\n\n"
    (Obook.Fixed.to_f report.filled_qty)
    (List.length report.fills);

  (* Print live ASCII depth & microstructure telemetry *)
  Obook.pp book
