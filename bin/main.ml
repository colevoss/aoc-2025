open Printf

let fname day t =
  "data/" ^ "day" ^ day ^ "-" ^ t ^ ".txt"

let () =
  let file = fname "three" "input" in
  let ic = open_in file in

  let answer = Aoc.Daythree.part_one ic in

  printf "Answer: %d\n" answer;

  close_in ic
