open Printf

let fname day t =
  "data/" ^ "day" ^ day ^ "-" ^ t ^ ".txt"

let () =
  let file = fname "one" "input" in
  let ic = open_in file in

  let answer = Aoc.Dayone.part_two ic in

  printf "Answer: %d\n" answer;

  close_in ic
