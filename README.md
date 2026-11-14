# Obook 

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.13%2B%20%7C%205.x-orange.svg)](https://ocaml.org)
[![Dependencies](https://img.shields.io/badge/Dependencies-Zero-brightgreen.svg)](#)

A fast, zero-dependency **Level-3 Limit Order Book** and **market microstructure math library** written in pure OCaml.


Most open-source order books are either weekend toy scripts that fall over when you feed them floating-point numbers (`0.1 + 0.2 = 0.30000000000000004`, RIP your PnL), or massive C++ monsters that take three hours to compile.

`Obook` sits in the sweet spot: **zero external dependencies**, **pre-allocated flat arrays** so the OCaml garbage collector stays out of your way, and a dead-simple API (`Obook.Ez`) so you can spin up an exchange matching engine in 5 lines of code.



---

## Quick Honesty Note (Read This First)

Let's be real: I built this project to bring a proper, plug-and-play order book and micro-structure library to the OCaml ecosystem, **and I have indeed used AI to help validate the complex maths and edge-case proofs** (128-bit integer division remainders, Welford's online variance stability, Cont's OFI, and VPIN bucket slicing) because I am frankly bad at doing arithmetic proofs by hand without killing myself.

Every formula and matching rule isn't just "trust me bro"ish and it's backed by an automated invariant checker (`Book.audit`) and a **50,000-event randomized fuzzer** that checks volume conservation and book state after every order. SO it's nice if you can suggest changes or better things to add to it. 

Truth be TOLD EVEN I DON"T KNOW HOW ALL OF THIS WORKS!!1

---

## Show Me The Money Shot (30-Second Quickstart)

If you don't want to mess with fixed-point integers or order IDs, just use the top-level `Obook` (powered by `Obook.Ez`). Pass normal floats and it handles the rest:

```ocaml
open Obook

let () =
  (* 1. Spin up a book *)
  let book = Obook.book "SOL-USD" in

  (* 2. Drop some passive Bids and Asks *)
  let _b1, _ = Obook.bid book ~px:180.25 ~qty:25.0 () in
  let _b2, _ = Obook.bid book ~px:180.00 ~qty:50.0 () in
  let _a1, _ = Obook.ask book ~px:180.50 ~qty:15.0 () in

  (* Sneaky Iceberg Ask: 100 SOL total, but only 10 SOL shows up on L2 at a time *)
  let _ice, _ = Obook.ask book ~px:180.75 ~qty:100.0 ~iceberg:10.0 () in

  (* 3. Fire a Market Buy that sweeps the best ask and bites into the iceberg *)
  let rep = Obook.buy_mkt book ~qty:20.0 () in
  Printf.printf "Filled %.2f SOL across %d trades!\n\n"
    (Fixed.to_f rep.filled_qty) (List.length rep.fills);

  (* 4. Print a clean ASCII depth table + live quant stats *)
  Obook.pp book
```

### What That Prints (VOILA):

```text
Filled 20.00 SOL across 2 trades!

==================================================================
 OBOOK L3 ENGINE  |  SOL-USD     |  Orders: 3      Trades: 2    
------------------------------------------------------------------
 Mid:  180.5000  |  MicroPx:  180.6667  |  Spread:    0.5000
 OFI:     15.00  |  VPIN:       n/a     |  Vol(EW):    0.0367
------------------------------------------------------------------
       BIDS (Qty @ Px)         |         ASKS (Px @ Qty)          
-------------------------------+----------------------------------
 [ 1]    25.0000 @ 180.25      | 180.75      @ 5.0000     [ 1]   
 [ 1]    50.0000 @ 180.00      |                                 
==================================================================
```

Want JSON for a web dashboard or WebSocket feed instead? One line:

```ocaml
let payload : string = Obook.Ez.to_json ~levels:5 book
```

---

## What's Under The Hood?

If you *do* care about low-level systems stuff, here is what `Obook` actually does:

* **`Obook.Fixed` (No Float Drift, No Overflows):**
  All prices and sizes are stored as 64-bit integers scaled by $10^8$ (1 satoshi / pipette). If you multiply $\$100,000$ by $1,000\text{ BTC}$ in naive `int64`, you hit $10^{24}$ and overflow `Int64.max_int`. `Fixed` splits numbers into 128-bit intermediate chunks so multiplication and division don't blow up, and supports

**Banker's Rounding (`Half_even`)**.
* **`Obook.Arena` (Zero-GC Order Pool):**

Pre-allocates flat arrays for resting orders plus an open-addressed hash table so looking up, cancelling, or shrinking an order by ID is strictly **$O(1)$** with zero 
heap allocation on the hot path.

* **`Obook.Book` (Real Exchange Rules):**
  * **Order Types:** `GTC`, `IOC`, `FOK`, and `Post_only` (maker-or-cancel).
  * **Zero-Mutation FOK Check:** Before executing a Fill-Or-Kill order, `Obook` does a read-only dry run across opposing price levels. If there isn't enough size, it rejects cleanly without mutating a single byte and trying to roll back.
 
  
  * **Iceberg Priority Law:** Partial fills of the displayed tip keep their timestamp priority. Once the tip hits zero, it refills from the hidden stash and moves to the back of the FIFO queue for that price level (standard CME/NASDAQ behavior).

* **Self-Trade Prevention (STP):** Stop wash trades with `Cancel_newest`, `Cancel_oldest`, or `Decrement_and_cancel`.
* **`Obook.Stats` (Streaming $O(1)$ Quant Math):**
No sliding arrays eating RAM. Includes **Welford's running variance** (with Chan's parallel merge), **Finch's EWMA volatility**, **Cont's Order Flow Imbalance (OFI)**, **VPIN** (Volume-Synchronized Probability of Informed Trading, with multi-bucket slicing when a whale sweeps the book), and **Roll's effective spread**.

* **`Obook.Itch` (NASDAQ ITCH 5.0 Wire Codec):**
Zero-copy big-endian binary parser and encoder (`S`, `A`, `E`, `X`, `D`, `U`) that feeds directly into `Book.t`.

---

## How Fast Is It?

Running `dune exec obook` on a single core (200,000 limit orders + cancels + 105,106 matched trades):

```text
>>> Obook Zero-Allocation Hot-Path Benchmark (200000 events) <<<
Elapsed Time   : 0.0833 s
Throughput     : 2400325 events/sec
Latency        : 416.6 ns/op
Trades Matched : 105106 (Vol: 582245.00)
Book Audit     : PASS (0 violations)
```

Around **2.4 million events/sec (~416 ns per operation)** in pure, type-safe cocaine.

---

## Install & Run

### Using Opam
```bash
opam pin add obook https://github.com/cryogenicdeadfrost/obook.git
```

In your `dune` file:
```lisp
(executable
 (name my_bot)
 (libraries obook))
```

### Local Development
```bash
git clone https://github.com/cryogenicdeadfrost/obook.git
cd obook

# Build everything
dune build

# Run unit tests + 50,000-event fuzzer
dune runtest

# Run the live demo + 200k-event benchmark
dune exec obook

# Run standalone examples
dune exec examples/quickstart.exe
dune exec examples/microstructure_toxic_flow.exe
```

---

## Project Layout

* [`lib/ez.mli`](lib/ez.mli) -- The chill, float-based API (`Obook.book`, `Obook.bid`, `Obook.ask`, `Obook.pp`, `Obook.Ez.to_json`).
* [`lib/book.mli`](lib/book.mli) -- Full L3 matching engine (`GTC`, `IOC`, `FOK`, `Post_only`, Iceberg, STP, `audit`).
* [`lib/fixed.mli`](lib/fixed.mli) -- $10^8$ fixed-point numbers & `Fixed.O` infix operators (`+~`, `-~`, `*~`, `/~`).
* [`lib/stats.mli`](lib/stats.mli) -- `Welford`, `Ewma`, `Ofi`, `Vpin`, and `Roll`.
* [`lib/itch.mli`](lib/itch.mli) -- NASDAQ TotalView-ITCH 5.0 binary encoder/decoder.
* [`docs/MATH_SPEC.md`](docs/MATH_SPEC.md) -- The nerd sheet with all the math formulas and invariants.

---

## License

MIT -- do whatever you want with it, just don't blame me if your trading bot fucks some shit up.
