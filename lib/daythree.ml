open Printf

let get_line_ints line =
  line
  |> String.to_seq
  |> Seq.map (fun c -> int_of_string (String.make 1 c))
  |> List.of_seq

let print_line line =
  line
  |> List.iter (fun i -> printf "%d " i);
  printf "\n"

let rec get_max_int_index (max, max_index) cur_index line =
  match line with
  | [] -> (max, max_index)
  | h :: tl ->
      let (new_max, new_max_index) = if h > max then
        (h, cur_index)
      else
        (max, max_index)
      in
      get_max_int_index (new_max, new_max_index) (cur_index + 1) tl

let bank_joltage_digits bank =
  let bank_len = List.length bank in
  let last_index = (bank_len - 1) in
  let max, max_index = get_max_int_index (0, 0) 0 bank in

  if max_index == last_index then
      let bank_head = List.take last_index bank in

      let first_digit, _ = get_max_int_index (0, 0) 0 bank_head in
      (first_digit, max)
  else
      let bank_tail = List.drop (max_index + 1) bank in

      let last_digit, _ = get_max_int_index (0, 0) 0 bank_tail in
      (max, last_digit)

let make_int_from_digits (hi, lo) =
  int_of_string (sprintf "%d%d" hi lo)

let part_one input =
  input
  |> Seq.unfold Read.read_lines
  |> Seq.map get_line_ints
  |> Seq.map bank_joltage_digits
  |> Seq.map make_int_from_digits
  |> Seq.fold_left (fun total i -> total + i) 0
