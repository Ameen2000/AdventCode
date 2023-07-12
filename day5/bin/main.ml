open Core

(* Advent of Code Day 5 *)
let file = "input.txt"

(* Part 1*)
type command =
  { amount : int
  ; from : int
  ; onto : int
  }

let parse_command input =
  let instructions = String.split input ~on:' ' |> Array.of_list in
  { amount = Int.of_string instructions.(1)
  ; from = Int.of_string instructions.(3) - 1
  ; onto = Int.of_string instructions.(5) - 1
  }
;;

let split_every_n n str =
  let length = String.length str in
  let rec aux i acc =
    if i >= length
    then List.rev acc
    else aux (i + n) (String.sub str ~pos:i ~len:(min (n - 1) (length - i)) :: acc)
  in
  aux 0 []
;;

let parse_stack stack =
  let ( = ) = String.( = ) in
  Str.split (Str.regexp " 1") stack
  |> List.hd_exn
  |> String.split_lines
  |> List.map ~f:(split_every_n 4)
  |> List.transpose_exn
  |> Array.of_list
  |> Array.map ~f:(List.drop_while ~f:(fun x -> x = "   "))
;;

let rec apply_commands stack instructions =
  match instructions with
  | [] -> ()
  | { amount; from; onto } :: rest ->
    let remove_from = stack.(from) in
    let add_to = stack.(onto) in
    let new_remove_from = List.drop remove_from amount in
    let taken_boxes = List.take remove_from amount |> List.rev in
    let new_add_to = taken_boxes @ add_to in
    stack.(from) <- new_remove_from;
    stack.(onto) <- new_add_to;
    apply_commands stack rest
;;

(* Part 2 *)
let rec apply_commands_2 stack instructions =
  match instructions with
  | [] -> ()
  | { amount; from; onto } :: rest ->
    let remove_from = stack.(from) in
    let add_to = stack.(onto) in
    let new_remove_from = List.drop remove_from amount in
    let taken_boxes = List.take remove_from amount in
    let new_add_to = taken_boxes @ add_to in
    stack.(from) <- new_remove_from;
    stack.(onto) <- new_add_to;
    apply_commands_2 stack rest
;;

(* Execution Block *)
let () =
  let content = In_channel.read_all file in
  let input = Str.split (Str.regexp "\n\n") content in
  let stacks = List.hd_exn input |> parse_stack in
  let stacks2 = List.hd_exn input |> parse_stack in
  let commands = List.last_exn input |> String.split_lines |> List.map ~f:parse_command in
  apply_commands stacks commands;
  apply_commands_2 stacks2 commands;
  let answer = Array.map ~f:List.hd_exn stacks in
  let answer2 = Array.map ~f:List.hd_exn stacks2 in
  Array.iter ~f:print_string answer;
  print_string "\n";
  Array.iter ~f:print_string answer2
;;
