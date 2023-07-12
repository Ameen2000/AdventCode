open Core

(* Advent of Code Day 6 *)
let file = "input.txt"

let solver input =
  let lst = String.to_list input in
  let rec aux lst count take =
    match lst with
    | [] -> count
    | _ :: t ->
      let code = List.take lst take in
      if List.contains_dup code ~compare:Char.compare
      then aux t (count + 1) take
      else count
  in
  aux lst 4 4, aux lst 14 14
;;

(* Execution block *)
let () =
  let input = In_channel.read_all file in
  let answer = solver input in
  Printf.printf "Answer to part 1 is: %d\n" (fst answer);
  Printf.printf "Answer to part 2 is: %d\n" (snd answer)
;;
