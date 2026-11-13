(lang dune 2.9)
(name obook)

(generate_opam_files true)

(authors "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(maintainers "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(homepage "https://github.com/cryogenicdeadfrost/obook")
(bug_reports "https://github.com/cryogenicdeadfrost/obook/issues")
(source (github cryogenicdeadfrost/obook))
(license MIT)

(package
 (name obook)
 (synopsis "Zero-allocation L3 limit order book and streaming HFT math in pure OCaml")
 (description "Obook is a dependency-free, zero-allocation L3 matching engine and market microstructure library for OCaml. It packs 128-bit intermediate fixed-point math, pre-allocated order arenas with O(1) cancel/amend, Iceberg/FOK/IOC/Post-Only/STP order logic, NASDAQ ITCH 5.0 wire parsing, and streaming quant indicators (Welford, EWMA, Cont OFI, VPIN, Roll spread).")
 (depends
  (ocaml (>= 4.13.0))
  (dune (>= 2.9))))
