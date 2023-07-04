open Core
(* Advent of Code Day 3 *)

(* Part 1 *)
let compartments input =
  let size = String.length input in
  let compartment1 =
    String.sub input ~pos:0 ~len:(size / 2)
    |> String.to_list
    |> Set.of_list (module Char)
  in
  let compartment2 =
    String.sub input ~pos:(size / 2) ~len:(size / 2)
    |> String.to_list
    |> Set.of_list (module Char)
  in
  let common =
    Set.inter compartment1 compartment2 |> Set.to_list |> List.hd_exn
  in
  let ( = ) = Char.( = ) in
  match common = Char.lowercase common with
  | true -> Char.to_int common - Char.to_int 'a' + 1
  | false -> Char.to_int common - Char.to_int 'A' + 27

(* Part 2*)
let badge_finder input =
  List.map ~f:String.to_list input
  |> List.map ~f:(Set.of_list (module Char))
  |> List.reduce_exn ~f:Set.inter
  |> Set.to_list |> List.hd_exn

let scorer input =
  let ( = ) = Char.( = ) in
  match input = Char.lowercase input with
  | true -> Char.to_int input - Char.to_int 'a' + 1
  | false -> Char.to_int input - Char.to_int 'A' + 27

let () =
  let file_contents = In_channel.read_all "rucksacks.txt" in
  let ordered = String.split_lines file_contents in
  let grouped = List.chunks_of ~length:3 ordered in
  let scores = List.map ~f:compartments ordered in
  let scores2 = List.map ~f:badge_finder grouped |> List.map ~f:scorer in
  let total = List.fold_right ~f:( + ) ~init:0 scores in
  let total2 = List.fold_right ~f:( + ) ~init:0 scores2 in
  Printf.printf "Answer to part 1 is: %d\n" total;
  Printf.printf "Answer to part 2 is: %d\n" total2
