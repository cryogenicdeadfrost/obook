## v1.0.0 (2026-11-14)

- Initial release of `obook`.
- `Obook.Fixed`: 64-bit fixed-point numbers ($10^{-8}$ scale) with 128-bit intermediate multiplication/division and Banker's rounding.
- `Obook.Arena`: Pre-allocated flat order pool + open-addressed hash table with zero-allocation tombstone compaction.
- `Obook.Book`: L3 Limit Order Book matching engine with GTC, IOC, FOK (read-only pre-scan), Post-Only, Iceberg FIFO replenishment, STP, and $O(1)$ cancel/amend.
- `Obook.Stats`: Streaming $O(1)$ microstructure metrics (`Welford`, `Ewma`, `Ofi`, `Vpin`, `Roll`).
- `Obook.Itch`: Zero-copy NASDAQ TotalView-ITCH 5.0 wire codec.
- `Obook.Ez`: Batteries-included float API, JSON telemetry export, and ASCII depth table printer.
