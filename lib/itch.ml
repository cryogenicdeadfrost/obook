open Fixed.O

type msg =
  | System_event of {
      ts : int64;
      code : char;
    }
  | Add_order of {
      ts : int64;
      oid : int;
      side : Book.side;
      qty : Fixed.t;
      symbol : string;
      px : Fixed.t;
    }
  | Order_executed of {
      ts : int64;
      oid : int;
      exec_qty : Fixed.t;
    }
  | Order_cancel of {
      ts : int64;
      oid : int;
      canc_qty : Fixed.t;
    }
  | Order_delete of {
      ts : int64;
      oid : int;
    }
  | Order_replace of {
      ts : int64;
      orig_oid : int;
      new_oid : int;
      new_qty : Fixed.t;
      new_px : Fixed.t;
    }

(* Standard ITCH 5.0 payload lengths by message tag *)
let msg_size = function
  | 'S' -> Some 12
  | 'A' -> Some 36
  | 'E' -> Some 31
  | 'X' -> Some 23
  | 'D' -> Some 19
  | 'U' -> Some 35
  | _ -> None

let get_u8 b off = Char.code (Bytes.get b off)

let set_u8 b off v = Bytes.set b off (Char.chr (v land 0xff))

let get_u32_be b off =
  let b0 = get_u8 b off in
  let b1 = get_u8 b (off + 1) in
  let b2 = get_u8 b (off + 2) in
  let b3 = get_u8 b (off + 3) in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let set_u32_be b off v =
  set_u8 b off ((v lsr 24) land 0xff);
  set_u8 b (off + 1) ((v lsr 16) land 0xff);
  set_u8 b (off + 2) ((v lsr 8) land 0xff);
  set_u8 b (off + 3) (v land 0xff)

(* ITCH 5.0 timestamps are 48-bit big-endian nanoseconds since midnight *)
let get_u48_be b off =
  let acc = ref 0L in
  for i = 0 to 5 do
    acc := Int64.logor (Int64.shift_left !acc 8) (Int64.of_int (get_u8 b (off + i)))
  done;
  !acc

let set_u48_be b off v =
  for i = 5 downto 0 do
    let shift = (5 - i) * 8 in
    set_u8 b (off + i) (Int64.to_int (Int64.logand (Int64.shift_right_logical v shift) 0xffL))
  done

let get_u64_be b off =
  let acc = ref 0L in
  for i = 0 to 7 do
    acc := Int64.logor (Int64.shift_left !acc 8) (Int64.of_int (get_u8 b (off + i)))
  done;
  !acc

let set_u64_be b off v =
  for i = 7 downto 0 do
    let shift = (7 - i) * 8 in
    set_u8 b (off + i) (Int64.to_int (Int64.logand (Int64.shift_right_logical v shift) 0xffL))
  done

let get_stock8 b off =
  let s = Bytes.sub_string b off 8 in
  String.trim s

let set_stock8 b off sym =
  Bytes.fill b off 8 ' ';
  let len = Int.min 8 (String.length sym) in
  Bytes.blit_string sym 0 b off len

(* ITCH 5.0 prices use 4 implied decimal places ($0.0001 per unit);
   multiply by 10^4 to align with Fixed.t's 10^8 scale. *)
let itch_px_scale = 10_000L

let px_of_itch32 raw32 =
  Fixed.of_raw (Int64.mul (Int64.of_int raw32) itch_px_scale)

let itch32_of_px px =
  Int64.to_int (Int64.div (Fixed.to_raw px) itch_px_scale)

let decode buf ~off =
  let blen = Bytes.length buf in
  if off < 0 || off >= blen then Error "Itch.decode: offset out of bounds"
  else
    let tag = Bytes.get buf off in
    match msg_size tag with
    | None -> Error (Printf.sprintf "Itch.decode: unknown message tag '%c'" tag)
    | Some sz ->
      if off + sz > blen then Error "Itch.decode: truncated frame"
      else (
        let ts = get_u48_be buf (off + 5) in
        match tag with
        | 'S' ->
          let code = Bytes.get buf (off + 11) in
          Ok (System_event { ts; code }, off + sz)
        | 'A' ->
          let oid = Int64.to_int (get_u64_be buf (off + 11)) in
          let side_ch = Bytes.get buf (off + 19) in
          let side : Book.side = if side_ch = 'B' then Bid else Ask in
          let shares = get_u32_be buf (off + 20) in
          let qty = Fixed.of_int_exn shares in
          let symbol = get_stock8 buf (off + 24) in
          let raw_px = get_u32_be buf (off + 32) in
          let px = px_of_itch32 raw_px in
          Ok (Add_order { ts; oid; side; qty; symbol; px }, off + sz)
        | 'E' ->
          let oid = Int64.to_int (get_u64_be buf (off + 11)) in
          let exec_qty = Fixed.of_int_exn (get_u32_be buf (off + 19)) in
          Ok (Order_executed { ts; oid; exec_qty }, off + sz)
        | 'X' ->
          let oid = Int64.to_int (get_u64_be buf (off + 11)) in
          let canc_qty = Fixed.of_int_exn (get_u32_be buf (off + 19)) in
          Ok (Order_cancel { ts; oid; canc_qty }, off + sz)
        | 'D' ->
          let oid = Int64.to_int (get_u64_be buf (off + 11)) in
          Ok (Order_delete { ts; oid }, off + sz)
        | 'U' ->
          let orig_oid = Int64.to_int (get_u64_be buf (off + 11)) in
          let new_oid = Int64.to_int (get_u64_be buf (off + 19)) in
          let new_qty = Fixed.of_int_exn (get_u32_be buf (off + 27)) in
          let new_px = px_of_itch32 (get_u32_be buf (off + 31)) in
          Ok (Order_replace { ts; orig_oid; new_oid; new_qty; new_px }, off + sz)
        | _ -> assert false
      )

let decode_all buf =
  let len = Bytes.length buf in
  let off = ref 0 in
  let acc = ref [] in
  let err = ref None in
  while !off < len && !err = None do
    match decode buf ~off:!off with
    | Ok (m, next_off) ->
      acc := m :: !acc;
      off := next_off
    | Error e -> err := Some e
  done;
  match !err with
  | Some e -> Error e
  | None -> Ok (List.rev !acc)

let encode m =
  match m with
  | System_event { ts; code } ->
    let b = Bytes.make 12 '\x00' in
    Bytes.set b 0 'S';
    set_u48_be b 5 ts;
    Bytes.set b 11 code;
    b
  | Add_order { ts; oid; side; qty; symbol; px } ->
    let b = Bytes.make 36 '\x00' in
    Bytes.set b 0 'A';
    set_u48_be b 5 ts;
    set_u64_be b 11 (Int64.of_int oid);
    Bytes.set b 19 (match side with Bid -> 'B' | Ask -> 'S');
    set_u32_be b 20 (Int.of_float (Fixed.to_f qty));
    set_stock8 b 24 symbol;
    set_u32_be b 32 (itch32_of_px px);
    b
  | Order_executed { ts; oid; exec_qty } ->
    let b = Bytes.make 31 '\x00' in
    Bytes.set b 0 'E';
    set_u48_be b 5 ts;
    set_u64_be b 11 (Int64.of_int oid);
    set_u32_be b 19 (Int.of_float (Fixed.to_f exec_qty));
    b
  | Order_cancel { ts; oid; canc_qty } ->
    let b = Bytes.make 23 '\x00' in
    Bytes.set b 0 'X';
    set_u48_be b 5 ts;
    set_u64_be b 11 (Int64.of_int oid);
    set_u32_be b 19 (Int.of_float (Fixed.to_f canc_qty));
    b
  | Order_delete { ts; oid } ->
    let b = Bytes.make 19 '\x00' in
    Bytes.set b 0 'D';
    set_u48_be b 5 ts;
    set_u64_be b 11 (Int64.of_int oid);
    b
  | Order_replace { ts; orig_oid; new_oid; new_qty; new_px } ->
    let b = Bytes.make 35 '\x00' in
    Bytes.set b 0 'U';
    set_u48_be b 5 ts;
    set_u64_be b 11 (Int64.of_int orig_oid);
    set_u64_be b 19 (Int64.of_int new_oid);
    set_u32_be b 27 (Int.of_float (Fixed.to_f new_qty));
    set_u32_be b 31 (itch32_of_px new_px);
    b

let encode_all msgs =
  let chunks = List.map encode msgs in
  Bytes.concat Bytes.empty chunks

let apply b = function
  | System_event { code = 'C'; _ } -> Book.clear b
  | System_event _ -> ()
  | Add_order { ts; oid; side; qty; px; _ } ->
    let _ = Book.add_limit b ~id:oid ~side ~px ~qty ~ts () in
    ()
  | Order_executed { ts; oid; exec_qty } ->
    (match Book.find_order b ~id:oid with
     | None -> ()
     | Some o ->
       let tot = o.disp_qty +~ o.hid_qty in
       let rem = if exec_qty >=~ tot then Fixed.zero else tot -~ exec_qty in
       let _ = Book.amend b ~id:oid ~new_px:o.px ~new_qty:rem ~ts () in
       ())
  | Order_cancel { ts; oid; canc_qty } ->
    (match Book.find_order b ~id:oid with
     | None -> ()
     | Some o ->
       let tot = o.disp_qty +~ o.hid_qty in
       let rem = if canc_qty >=~ tot then Fixed.zero else tot -~ canc_qty in
       let _ = Book.amend b ~id:oid ~new_px:o.px ~new_qty:rem ~ts () in
       ())
  | Order_delete { oid; _ } ->
    let _ = Book.cancel b ~id:oid in
    ()
  | Order_replace { ts; orig_oid; new_oid; new_qty; new_px } ->
    (match Book.find_order b ~id:orig_oid with
     | None -> ()
     | Some o ->
       let _ = Book.cancel b ~id:orig_oid in
       let _ = Book.add_limit b ~id:new_oid ~trader:o.trader ~side:o.side ~px:new_px ~qty:new_qty ~ts () in
       ())
