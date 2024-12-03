(*[@@@warning "-26-27"]*)

open Core
open Angstrom

type command =
  | On
  | Off
  | Mul of int * int

let input_file = "input/3.txt"
let input () = In_channel.read_all input_file

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let parse_mul =
  let* _ = string "mul" in
  let* _ = string "(" in
  let* first = integer in
  let* _ = string "," in
  let* second = integer in
  let* _ = string ")" in
  return (Mul (first, second))
;;

let skip_till p t = fix (fun m -> t <|> p *> m)
let seek = skip_till (advance 1)
let parse_on = string "do()" >>| fun _ -> On
let parse_off = string "don't()" >>| fun _ -> Off
let parse_command = parse_on <|> parse_off <|> parse_mul
let find_command = seek parse_command
let find_mul = seek parse_mul

let find_mul_stand_alone =
  seek parse_mul
  >>| fun c ->
  match c with
  | Mul (a, b) -> a, b
  | _ -> failwith "impossible"
;;

let part_1 () =
  let text = input () in
  let muls =
    parse_string ~consume:Prefix (many find_mul_stand_alone) text |> Result.ok_or_failwith
  in
  let result =
    muls |> List.map ~f:(fun (a, b) -> a * b) |> List.sum (module Int) ~f:Fun.id
  in
  printf "%d\n" result;
  ()
;;

let evaluate_commands commands =
  let rec inner on remaining result =
    match remaining with
    | [] -> result
    | next :: remaining ->
      (match on, next with
       | true, Mul (a, b) -> inner on remaining (result + (a * b))
       | false, Mul _ -> inner on remaining result
       | _, On -> inner true remaining result
       | _, Off -> inner false remaining result)
  in
  inner true commands 0
;;

let part_2 () =
  let text = input () in
  let commands =
    parse_string ~consume:Prefix (many find_command) text |> Result.ok_or_failwith
  in
  let result = evaluate_commands commands in
  printf "%d\n" result;
  ()
;;
