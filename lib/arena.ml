type side =
  | Bid
  | Ask

let null = -1

type t = {
  cap : int;
  mutable active : int;
  mutable free_head : int;
  alive : bool array;
  id : int array;
  trader : int array;
  side : side array;
  px : Fixed.t array;
  disp_qty : Fixed.t array;
  hid_qty : Fixed.t array;
  peak_qty : Fixed.t array;
  ts : int64 array;
  prev : int array;
  next : int array;
  level_idx : int array;
  ht_size : int;
  ht_mask : int;
  ht_keys : int array;
  ht_slots : int array;
  ht_state : int array; (* 0 = empty, 1 = occupied, 2 = tombstone *)
  mutable ht_tombstones : int;
}

let next_pow2 n =
  let p = ref 16 in
  while !p < n do
    p := !p lsl 1
  done;
  !p

let hash_id k mask =
  (* Murmur3-style avalanche so sequential order IDs don't cluster *)
  let h = k lxor (k lsr 16) in
  let h = h * 0x45d9f3b in
  let h = h lxor (h lsr 16) in
  h land mask

let create ~capacity =
  if capacity < 1 then invalid_arg "Arena.create: capacity must be >= 1";
  let ht_size = next_pow2 (capacity * 4) in
  let next_arr = Array.init capacity (fun i -> if i + 1 < capacity then i + 1 else null) in
  {
    cap = capacity;
    active = 0;
    free_head = 0;
    alive = Array.make capacity false;
    id = Array.make capacity 0;
    trader = Array.make capacity 0;
    side = Array.make capacity Bid;
    px = Array.make capacity Fixed.zero;
    disp_qty = Array.make capacity Fixed.zero;
    hid_qty = Array.make capacity Fixed.zero;
    peak_qty = Array.make capacity Fixed.zero;
    ts = Array.make capacity 0L;
    prev = Array.make capacity null;
    next = next_arr;
    level_idx = Array.make capacity null;
    ht_size;
    ht_mask = ht_size - 1;
    ht_keys = Array.make ht_size 0;
    ht_slots = Array.make ht_size null;
    ht_state = Array.make ht_size 0;
    ht_tombstones = 0;
  }

let clear a =
  a.active <- 0;
  a.free_head <- 0;
  a.ht_tombstones <- 0;
  for i = 0 to a.cap - 1 do
    a.alive.(i) <- false;
    a.prev.(i) <- null;
    a.next.(i) <- (if i + 1 < a.cap then i + 1 else null);
    a.level_idx.(i) <- null
  done;
  Array.fill a.ht_state 0 a.ht_size 0

let capacity a = a.cap
let active_count a = a.active

let ht_put_raw a key slot =
  let idx = ref (hash_id key a.ht_mask) in
  let first_tomb = ref null in
  let found = ref false in
  while not !found do
    let st = a.ht_state.(!idx) in
    if st = 0 then (
      let target = if !first_tomb <> null then (
        a.ht_tombstones <- a.ht_tombstones - 1;
        !first_tomb
      ) else !idx in
      a.ht_keys.(target) <- key;
      a.ht_slots.(target) <- slot;
      a.ht_state.(target) <- 1;
      found := true
    ) else if st = 2 then (
      if !first_tomb = null then first_tomb := !idx;
      idx := (!idx + 1) land a.ht_mask
    ) else if a.ht_keys.(!idx) = key then (
      a.ht_slots.(!idx) <- slot;
      found := true
    ) else (
      idx := (!idx + 1) land a.ht_mask
    )
  done

(* Zero-allocation tombstone sweep: since the arena already knows which slots are alive,
   we just wipe ht_state and re-index live slots in place when tombstones get noisy. *)
let compact_tombstones a =
  Array.fill a.ht_state 0 a.ht_size 0;
  a.ht_tombstones <- 0;
  for s = 0 to a.cap - 1 do
    if a.alive.(s) then ht_put_raw a a.id.(s) s
  done

let find_slot a key =
  let idx = ref (hash_id key a.ht_mask) in
  let res = ref null in
  let stop = ref false in
  let probes = ref 0 in
  while not !stop && !probes < a.ht_size do
    let st = a.ht_state.(!idx) in
    if st = 0 then stop := true
    else if st = 1 && a.ht_keys.(!idx) = key then (
      res := a.ht_slots.(!idx);
      stop := true
    ) else (
      idx := (!idx + 1) land a.ht_mask;
      incr probes
    )
  done;
  !res

let alloc a ~id ~trader ~side ~px ~disp_qty ~hid_qty ~peak_qty ~ts =
  if find_slot a id <> null then Error `Duplicate_id
  else if a.free_head = null then Error `Arena_full
  else (
    if a.ht_tombstones > a.ht_size lsr 2 then compact_tombstones a;
    let slot = a.free_head in
    a.free_head <- a.next.(slot);
    a.active <- a.active + 1;
    a.alive.(slot) <- true;
    a.id.(slot) <- id;
    a.trader.(slot) <- trader;
    a.side.(slot) <- side;
    a.px.(slot) <- px;
    a.disp_qty.(slot) <- disp_qty;
    a.hid_qty.(slot) <- hid_qty;
    a.peak_qty.(slot) <- peak_qty;
    a.ts.(slot) <- ts;
    a.prev.(slot) <- null;
    a.next.(slot) <- null;
    a.level_idx.(slot) <- null;
    ht_put_raw a id slot;
    Ok slot
  )

let free a slot =
  if slot >= 0 && slot < a.cap && a.alive.(slot) then (
    let key = a.id.(slot) in
    let idx = ref (hash_id key a.ht_mask) in
    let done_ht = ref false in
    let probes = ref 0 in
    while not !done_ht && !probes < a.ht_size do
      let st = a.ht_state.(!idx) in
      if st = 0 then done_ht := true
      else if st = 1 && a.ht_keys.(!idx) = key then (
        let next_idx = (!idx + 1) land a.ht_mask in
        if a.ht_state.(next_idx) = 0 then a.ht_state.(!idx) <- 0
        else (
          a.ht_state.(!idx) <- 2;
          a.ht_tombstones <- a.ht_tombstones + 1
        );
        done_ht := true
      ) else (
        idx := (!idx + 1) land a.ht_mask;
        incr probes
      )
    done;
    a.alive.(slot) <- false;
    a.prev.(slot) <- null;
    a.level_idx.(slot) <- null;
    a.next.(slot) <- a.free_head;
    a.free_head <- slot;
    a.active <- a.active - 1
  )

let is_alive a s = a.alive.(s)
let id a s = a.id.(s)
let trader a s = a.trader.(s)
let side a s = a.side.(s)
let px a s = a.px.(s)
let disp_qty a s = a.disp_qty.(s)
let hid_qty a s = a.hid_qty.(s)
let peak_qty a s = a.peak_qty.(s)
let total_qty a s = Fixed.add_exn a.disp_qty.(s) a.hid_qty.(s)
let ts a s = a.ts.(s)
let prev a s = a.prev.(s)
let next a s = a.next.(s)
let level_idx a s = a.level_idx.(s)

let set_px a s v = a.px.(s) <- v
let set_disp_qty a s v = a.disp_qty.(s) <- v
let set_hid_qty a s v = a.hid_qty.(s) <- v
let set_ts a s v = a.ts.(s) <- v
let set_prev a s v = a.prev.(s) <- v
let set_next a s v = a.next.(s) <- v
let set_level_idx a s v = a.level_idx.(s) <- v
