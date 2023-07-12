open Core

(* Advent of Code Day 2*)
let file = "rock.txt"

(* Part 1 *)
type t =
  | Rock
  | Paper
  | Scissors

let hand str =
  match str with
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | x -> raise (Invalid_argument x)
;;

let result str =
  let theirs = String.sub ~pos:0 ~len:1 str in
  let ours = String.sub ~pos:2 ~len:1 str in
  match hand theirs, hand ours with
  | Rock, Rock -> 4
  | Rock, Paper -> 8
  | Rock, Scissors -> 3
  | Paper, Paper -> 5
  | Paper, Rock -> 1
  | Paper, Scissors -> 9
  | Scissors, Scissors -> 6
  | Scissors, Rock -> 7
  | Scissors, Paper -> 2
;;

(* Part 2 *)
type outcome =
  | Win
  | Draw
  | Loss

let theirs str =
  match str with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> assert false
;;

let outcome_of_string str =
  match str with
  | "X" -> Loss
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> assert false
;;

let scorer str =
  let their = theirs @@ String.sub str ~pos:0 ~len:1 in
  let outcome = outcome_of_string @@ String.sub str ~pos:2 ~len:1 in
  let ours ~their ~outcome =
    match their, outcome with
    | Rock, Win -> Paper, 8
    | Rock, Loss -> Scissors, 3
    | Rock, Draw -> Rock, 4
    | Paper, Win -> Scissors, 9
    | Paper, Loss -> Rock, 1
    | Paper, Draw -> Paper, 5
    | Scissors, Win -> Rock, 7
    | Scissors, Loss -> Paper, 2
    | Scissors, Draw -> Scissors, 6
  in
  snd @@ ours ~their ~outcome
;;

let () =
  let contents = In_channel.read_all file in
  let ordered = String.split ~on:'\n' contents in
  let size = List.length ordered in
  let results = List.map ~f:result (List.take ordered (size - 1)) in
  let results2 = List.map ~f:scorer (List.take ordered (size - 1)) in
  let total = List.fold_left ~f:( + ) ~init:0 results in
  let total2 = List.fold_left ~f:( + ) ~init:0 results2 in
  Printf.printf "Answer to Part 1: %d\n" total;
  Printf.printf "Answer to Part 2: %d\n" total2
;;
