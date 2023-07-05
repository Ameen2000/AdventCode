open Core

(* Advent of Code Day 4 *)
let file = "cleanup.txt"

(* Part 1 *)
let parse_interval input =
  match String.split input ~on:'-' with
  | [ first; second ] -> (Int.of_string first, Int.of_string second)
  | _ -> assert false

let parse_pair input =
  match String.split input ~on:',' with
  | [ first; second ] -> (parse_interval first, parse_interval second)
  | _ -> assert false

let in_range (x1, y1) (x2, y2) =
  if x1 <= x2 && y2 <= y1 then true
  else if x2 <= x1 && y1 <= y2 then true
  else false

let filterer parsed_input = in_range (fst parsed_input) (snd parsed_input)

(* Part 2*)
let overlaps (x1, y1) (x2, y2) =
  if x1 >= x2 && x1 <= y2 then true
  else if y1 >= x2 && y1 <= y2 then true
  else if x2 >= x1 && x2 <= y1 then true
  else if y2 >= x1 && y2 <= y1 then true
  else false

let filterer2 (x, y) = overlaps x y

(* Execution block *)
let () =
  let content = In_channel.read_all file in
  let ordered = String.split_lines content in
  let answer =
    List.map ~f:parse_pair ordered |> List.filter ~f:filterer |> List.length
  in
  let answer2 =
    List.map ~f:parse_pair ordered |> List.filter ~f:filterer2 |> List.length
  in
  Printf.printf "Answer to part 1: %d\n" answer;
  Printf.printf "Answer to part 2: %d\n" answer2
