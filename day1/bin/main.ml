open Core

(* Advent of Code day 1 *)
let file = "calories.txt"

(* Part 1 *)
let rec group input result =
  match input with
  | [] -> result
  | "" :: xs -> group xs (0 :: result)
  | x :: xs ->
      let y =
        match result with
        | [] -> [ int_of_string x ]
        | h :: t -> (h + int_of_string x) :: t
      in
      group xs y

let max_list lst =
  let rec aux curr = function
    | [] -> curr
    | hd :: tl -> if hd > curr then aux hd tl else aux curr tl
  in
  aux 0 lst

let () =
  let contents = In_channel.read_all file in
  let input = String.split ~on:'\n' contents in
  let ordered = group input [] in
  let result1 = max_list ordered in
  let sorted = List.sort ~compare:Int.descending ordered in
  let result2 = List.fold_right ~f:( + ) ~init:0 @@ List.take sorted 3 in
  Printf.printf "Answer to part 1 is: %d\n" result1;
  Printf.printf "Anwer to part2 is: %d\n" result2
