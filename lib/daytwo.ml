let get_part_ids range =
  let parts = String.split_on_char '-' range in
  (
    int_of_string (String.trim(List.hd parts)),
    int_of_string (String.trim(List.nth parts 1))
  )

let is_id_invalid id =
  let id_str = string_of_int id in
  let len = String.length id_str in

  if len mod 2 == 0 then
    let first = String.sub id_str 0 (len / 2) in
    let last = String.sub id_str (len / 2) (len / 2) in
    String.equal first last

  else
    false

let get_invalid_ids_from_range (lo, hi) validator =
  lo
  |> Seq.ints
  |> Seq.take ((hi - lo) + 1)
  |> Seq.filter validator
  |> Seq.fold_left (fun total i -> total + i) 0

let process_input input validator =
  input
  |> Read.read_all_lines
  |> String.split_on_char ','
  |> List.to_seq
  |> Seq.map get_part_ids
  |> Seq.map (fun range -> get_invalid_ids_from_range range validator)
  |> Seq.fold_left (fun total i -> total + i) 0


let rec does_part_repeat_in_string part s =
  let part_len = String.length part in
  let str_len = String.length s in
  let too_long = part_len > str_len in
  let cant_repeat = str_len mod part_len != 0 in

  (* Printf.printf "part: %s, s: %s\n" part s; *)

  if too_long || cant_repeat then
    false
  else
    let head = String.sub s 0 part_len in
    (* Printf.printf "hd: %s\n" head; *)
    if str_len == part_len then
      String.equal part head
    else
      let tail = String.sub s part_len (str_len - part_len) in

      (* Printf.printf "tl: %s\n" tail; *)

      if not (String.equal part head) then
        false
      else
        does_part_repeat_in_string part tail

let rec cycle_parts id part_size max_part_size =
  let str_len = String.length id in
  let head = String.sub id 0 part_size in
  let tail = String.sub id part_size (str_len - part_size) in

  if part_size > max_part_size then
    false
  else
    if does_part_repeat_in_string head tail then
      true
    else
      cycle_parts id (part_size + 1) max_part_size

let part_two_validator id_int =
  let id = string_of_int id_int in
  let max_chunk_len = (String.length id) / 2 in

  cycle_parts id 1 max_chunk_len

let test_does_part_repeat id =
  (* let does_repeat = does_part_repeat_in_string part str in *)
  let does_repeat = part_two_validator id in
  Printf.printf "%d valid: %b\n" id does_repeat

let part_one input =
  process_input input is_id_invalid

let part_two input =
  (* test_does_part_repeat 123123; *)
  (* test_does_part_repeat 11; *)
  (* test_does_part_repeat 112; *)
  (* test_does_part_repeat 446446; *)
  process_input input part_two_validator
