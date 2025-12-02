type direction =
  | Left of int
  | Right of int

exception InvalidDirection of string

let parse_line line =
  let dir_str = String.sub line 0 1 in
  let clicks = int_of_string (String.sub line 1 ((String.length line) - 1)) in

  match dir_str with
  | "L" -> Left clicks
  | "R" -> Right clicks
  | _ -> raise (InvalidDirection dir_str)

let turn_to_clicks = function
  | Left clicks -> -clicks
  | Right clicks -> clicks

let string_of_turn = function
  | Left clicks -> Printf.sprintf "L%d" clicks
  | Right clicks -> Printf.sprintf "R%d" clicks

let turn_dial dial turn =
  let clicks = (turn_to_clicks turn) mod 100 in

  match dial + clicks with
  | d when d < 0 -> 100 + d
  | d when d > 99 -> d - 100
  | d -> d

let part_one input =
  let (count, _dial) = input
  |> Seq.unfold Read.read_lines
  |> Seq.map parse_line
  |> Seq.fold_left (fun (count, dial) turn ->
      match turn_dial dial turn with
      | 0 -> (count + 1, 0)
      | d -> (count, d)
      ) (0, 50)
  in
  
  count

let turn_dial_track_zero dial turn =
  let clicks = turn_to_clicks turn in
  let full_turns = (Int.abs clicks) / 100 in
  let remaining_clicks = clicks mod 100 in
  let starts_at_zero = if dial == 0 then 0 else 1 in
  let crosses = full_turns + starts_at_zero in

  match dial + remaining_clicks with
  (* does this mean we cross zero? *)
  | 0 -> (crosses, 0)
  | d when d < 0 -> (crosses, 100 + d)
  | d when d > 99 -> (crosses, d - 100)
  | d -> (full_turns, d)

let part_two input =
  let (count, _dial) = input
  |> Seq.unfold Read.read_lines
  |> Seq.map parse_line
  |> Seq.fold_left (fun (count, dial) turn ->
    Printf.printf "count: %d, dial: %d, turn: %s\n" count dial (string_of_turn turn);

    match turn_dial_track_zero dial turn with
    | (turns, 0) -> (count + turns, 0)
    | (turns, d) -> (count + turns, d)

    ) (0, 50)
  in
  Printf.printf "count: %d, dial: %d\n" count _dial;
  count
