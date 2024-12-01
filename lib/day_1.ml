open Core

let input_file = "./input/1.txt"

let split_line line =
  let line = String.substr_replace_all ~pattern:"   " ~with_:" " line in
  match String.split ~on:' ' line with
  | first :: second :: _ -> first, second
  | _ -> failwith "unexpected line format"
;;

let split_list lst =
  let rec inner left right remaining =
    match remaining with
    | [] -> List.rev left, List.rev right
    | hd :: remaining ->
      let left_val, right_val = split_line hd in
      inner (left_val :: left) (right_val :: right) remaining
  in
  inner [] [] lst
;;

let input () =
  let lines = In_channel.read_lines input_file in
  let left, right = split_list lines in
  let mapped_left = List.map ~f:Int.of_string left in
  let mapped_right = List.map ~f:Int.of_string right in
  mapped_left, mapped_right
;;

let diff left right =
  let rec inner left right diff =
    match left, right with
    | [], [] -> diff
    | left_val :: left, right_val :: right ->
      inner left right (Int.abs (left_val - right_val) + diff)
    | _ -> failwith "list lengths are unequal"
  in
  inner left right 0
;;

let part_1 () =
  let left, right = input () in
  let sorted_left = left |> List.sort ~compare in
  let sorted_right = right |> List.sort ~compare in
  let diff =
    List.zip_exn sorted_left sorted_right
    |> List.map ~f:(fun (left, right) -> Int.abs (left - right))
    |> List.sum ~f:Fn.id (module Int)
  in
  Printf.printf "%d\n" diff
;;

let count_map lst =
  let opt_add_1 = Option.value_map ~default:1 ~f:(( + ) 1) in
  let rec inner counts remaining =
    match remaining with
    | [] -> counts
    | hd :: remaining ->
      let counts = Map.update ~f:opt_add_1 counts hd in
      inner counts remaining
  in
  inner (Map.empty (module Int)) lst
;;

let similarity lst counts =
  let score map i =
    match Map.find map i with
    | Some x -> i * x
    | None -> 0
  in
  lst |> List.map ~f:(score counts) |> List.sum ~f:Fun.id (module Int)
;;

let part_2 () =
  let left, right = input () in
  let right_counts = count_map right in
  let score = similarity left right_counts in
  Printf.printf "%d\n" score;
  ()
;;
