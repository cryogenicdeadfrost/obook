(lang dune 2.9)
(name obook)

(generate_opam_files true)

(authors "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(maintainers "Abhradeep Basak <abhradeepbasak07@gmail.com>")
(source (github cryogenicdeadfrost/obook))
(license MIT)

(package
 (name obook)
 (synopsis "Zero-allocation L3 limit order book, 128-bit fixed-point math, and streaming HFT microstructure in pure OCaml")
 (description "Obook is a dependency-free, zero-allocation L3 matching engine and quantitative microstructure toolkit for OCaml. Features overflow-proof 64-bit fixed-point arithmetic (10^-8 scale), pre-allocated arena order books with O(1) cancel/modify, Iceberg/FOK/IOC/Post-Only/STP execution semantics, NASDAQ ITCH 5.0 wire parsing, and numerically stable streaming indicators (Welford, Finch EWMA, Cont OFI, VPIN, Roll spread).")
 (depends
  (ocaml (>= 4.13.0))
  (dune (>= 2.9))))
