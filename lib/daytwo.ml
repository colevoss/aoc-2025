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

let get_invalid_ids_from_range (lo, hi) =
  lo
  |> Seq.ints
  |> Seq.take ((hi - lo) + 1)
  |> Seq.filter is_id_invalid
  |> Seq.fold_left (fun total i -> total + i) 0

let part_one input =
  input
  |> Read.read_all_lines
  |> String.split_on_char ','
  |> List.to_seq
  |> Seq.map get_part_ids
  |> Seq.map get_invalid_ids_from_range
  |> Seq.fold_left (fun total i -> total + i) 0
