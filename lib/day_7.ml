open Core
open Angstrom

let input_file = "input/7.txt"
let test_input_file = "input/7_test.txt"

let p_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| Int64.of_string
;;

let p_line =
  let* target = p_integer in
  let* _ = char ':' *> char ' ' in
  let* numbers = sep_by1 (char ' ') p_integer in
  return (target, numbers)
;;

let p_file = sep_by1 (char '\n') p_line

let input file =
  let text = In_channel.read_all file in
  parse_string ~consume:Prefix p_file text |> Result.ok_or_failwith
;;

let int_list_to_string = List.to_string ~f:Int64.to_string

let concat current next =
  Int64.of_string (sprintf "%s%s" (Int64.to_string current) (Int64.to_string next))
;;

let can_hit_target (target : int64) (equations : int64 list) with_concat =
  let rec inner current remaining =
    match remaining with
    | [] -> Int64.( = ) current target
    | next :: remaining ->
      inner (Int64.( + ) current next) remaining
      || inner (Int64.( * ) current next) remaining
      || (with_concat && inner (concat current next) remaining)
  in
  match equations with
  | [] -> false
  | [ value ] -> Int64.( = ) value target
  | current :: remaining -> inner current remaining
;;

let part_1 () =
  let equations = input input_file in
  let filtered =
    equations
    |> List.filter ~f:(fun (target, values) -> can_hit_target target values false)
  in
  let result =
    filtered
    |> List.fold ~init:(Int64.of_int 0) ~f:(fun sum (target, _) -> Int64.( + ) sum target)
  in
  printf "%s\n" (Int64.to_string result)
;;

let part_2 () =
  let equations = input input_file in
  let filtered =
    equations
    |> List.filter ~f:(fun (target, values) -> can_hit_target target values true)
  in
  let result =
    filtered
    |> List.fold ~init:(Int64.of_int 0) ~f:(fun sum (target, _) -> Int64.( + ) sum target)
  in
  printf "%s\n" (Int64.to_string result)
;;
