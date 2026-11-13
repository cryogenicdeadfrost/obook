# Obook — Zero-Allocation L3 Order Book & Streaming Microstructure in Pure OCaml

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![Dependencies](https://img.shields.io/badge/Dependencies-Zero-brightgreen.svg)](#)

**`Obook`** is a dependency-free, zero-allocation Level-3 (L3) Limit Order Book matching engine, 128-bit intermediate fixed-point arithmetic library, NASDAQ TotalView-ITCH 5.0 wire codec, and $O(1)$ streaming market micro-structure toolkit written in pure OCaml.

---

## Why `Obook`?

1. **Zero GC Pressure on the Matching Hot Path:**
   All resting orders and price levels live in pre-allocated flat arena arrays (`Obook.Arena`) indexed by an open-addressed linear-probed hash table with in-place zero-allocation tombstone compaction. Cancelling or reducing an order's size is strictly **$O(1)$ worst-case time**.
   
3. **Overflow-Proof 128-Bit Fixed-Point Math (`Obook.Fixed`):**
   Prices and quantities are signed 64-bit integers scaled by $10^8$ (1 satoshi / pipette). Multiplying $\$100,000$ by $1,000\text{ BTC}$ ($10^{24}$ raw units) overflows standard `int64`—`Obook.Fixed` decomposes multiplication and division into 128-bit intermediate limbs with deterministic **Banker's Rounding (`Half_even`)**, `Down`, `Up`, and `Zero` modes.
   
4. **Exchange-Grade Execution Semantics (`Obook.Book`):**
   * **Time-in-Force:** `GTC`, `IOC`, `FOK` (via a **zero-mutation read-only pre-scan** so failed FOKs never touch the arena), and `Post_only` (maker-or-cancel).
   * **Iceberg Orders:** Hidden reserve + displayed peak size. Partial display fills keep time priority; exhausted display slices replenish from hidden reserve and move to the tail of the FIFO price level queue per CME/NASDAQ rules.
   * **Self-Trade Prevention (STP):** `Cancel_newest`, `Cancel_oldest`, and `Decrement_and_cancel`.
   * **Priority-Preserving Amends:** Shrinking order size at the same price updates in-place in $O(1)$ time without losing queue position.
  
5. **Numerically Stable $O(1)$ Microstructure Analytics (`Obook.Stats`):**

   * **Welford + Chan Parallel Merge:** Running mean, variance, standard deviation, and Z-score immune to IEEE-754 catastrophic cancellation.
   * **Finch Bias-Free EWMA:** Exponentially weighted moving mean, variance, and volatility.
   * **Cont’s Order Flow Imbalance (OFI):** BBO supply/demand pressure estimator.
   * **VPIN (Volume-Synchronized Probability of Informed Trading):** Exact multi-bucket trade-slicing loop that keeps $\text{VPIN} \in [0, 1]$ even when a single whale sweep spans multiple volume buckets.
   * **Roll’s Effective Spread:** Serial price-change autocovariance estimator $s = 2\sqrt{-\text{Cov}(\Delta P_t, \Delta P_{t-1})}$.

7. **NASDAQ TotalView-ITCH 5.0 Binary Wire Codec (`Obook.Itch`):**
   Zero-copy big-endian encoder/decoder (`S`, `A`, `E`, `X`, `D`, `U`) that drives a `Book.t` directly from network buffers.

---

## 30-Second Quickstart (`Obook.Ez`)

While `Obook.Book` and `Obook.Fixed` give low-level systems control, the top-level `Obook` / `Obook.Ez` interface lets you spin up a book, submit float orders, and inspect telemetry in a few lines:

```ocaml
open Obook

let () =
  let book = Obook.book "SOL-USD" in

  (* Post passive Bids & Asks (plus an Iceberg order hiding 90 SOL) *)
  let _b1, _ = Obook.bid book ~px:180.25 ~qty:25.0 () in
  let _b2, _ = Obook.bid book ~px:180.00 ~qty:50.0 () in
  let _a1, _ = Obook.ask book ~px:180.50 ~qty:15.0 () in
  let _ice, _ = Obook.ask book ~px:180.75 ~qty:100.0 ~iceberg:10.0 () in

  (* Sweep with a Market Buy *)
  let rep = Obook.buy_mkt book ~qty:20.0 () in
  Printf.printf "Filled %.2f SOL across %d fills\n"
    (Fixed.to_f rep.filled_qty) (List.length rep.fills);

  (* Print ASCII L2 depth table & live microstructure telemetry *)
  Obook.pp book
```

### Output

```text
==================================================================
 OBOOK L3 ENGINE  |  SOL-USD     |  Orders: 3      Trades: 2    
------------------------------------------------------------------
 Mid:  180.5000  |  MicroPx:  180.6071  |  Spread:    0.5000
 OFI:    -15.00  |  VPIN:        n/a    |  Vol(EW):   0.0772
------------------------------------------------------------------
       BIDS (Qty @ Px)         |         ASKS (Px @ Qty)          
-------------------------------+----------------------------------
 [ 1]    25.0000 @ 180.25      | 180.75      @ 5.0000     [ 1]   
 [ 1]    50.0000 @ 180.00      |                                  
==================================================================
```

---

## Building, Testing & Running Benchmarks

```bash
# Build library and CLI executable
dune build

# Run invariant, property, and 50,000-event randomized fuzz tests
dune runtest

# Run live demo & 200,000-event hot-path throughput benchmark
dune exec obook

# Run standalone examples
dune exec examples/quickstart.exe
dune exec examples/microstructure_toxic_flow.exe
```

---

## Module Architecture

| Module | Responsibility |
| :--- | :--- |
| [`Obook.Fixed`](lib/fixed.mli) | 64-bit fixed-point arithmetic ($10^{-8}$ scale) with 128-bit intermediate `mul`/`div` and Banker's rounding. |
| [`Obook.Arena`](lib/arena.mli) | Pre-allocated order node pool + open-addressed hash table with zero-allocation tombstone compaction. |
| [`Obook.Book`](lib/book.mli) | L3 Limit Order Book engine (`GTC`, `IOC`, `FOK`, `Post_only`, Iceberg, STP, $O(1)$ `cancel`/`amend`, `audit`). |
| [`Obook.Stats`](lib/stats.mli) | Streaming $O(1)$ indicators: `Welford`, `Ewma`, `Ofi`, `Vpin`, and `Roll`. |
| [`Obook.Itch`](lib/itch.mli) | Zero-copy NASDAQ TotalView-ITCH 5.0 binary packet encoder, decoder, and book driver. |
| [`Obook.Ez`](lib/ez.mli) | Ergonomic float-based wrapper, JSON serializer, and ASCII L2 depth renderer. |

See [`docs/MATH_SPEC.md`](docs/MATH_SPEC.md) for the complete mathematical derivations and invariant proofs.
