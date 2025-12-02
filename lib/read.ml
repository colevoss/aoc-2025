let read_lines chan =
  try Some (input_line chan, chan)
  with End_of_file -> None

let parse_line line =
  String.split_on_char ' ' line
  |> List.to_seq
  |> Seq.filter (fun s -> String.length s > 0)

let parse_int_line line =
  parse_line line
  |> Seq.map int_of_string
