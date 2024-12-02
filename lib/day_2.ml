open Core

let input_file = "./input/2.txt"
let parse_report line = line |> String.split ~on:' ' |> List.map ~f:Int.of_string

let input () =
  let lines = In_channel.read_lines input_file in
  let reports = List.map ~f:parse_report lines in
  reports
;;

type progression =
  | Unknown
  | Increasing
  | Decreasing
  | Static

let get_progression first second =
  if first < second then Increasing else if second < first then Decreasing else Static
;;

let safe_level progression current next =
  let current_progression = get_progression current next in
  let safe_progression =
    (phys_equal progression Unknown && phys_equal progression Static |> not)
    || phys_equal progression current_progression
  in
  let diff = Int.abs (next - current) in
  safe_progression && diff >= 1 && diff <= 3
;;

let evaluate_report report =
  let rec inner progression current remaining =
    match remaining with
    | [] -> true
    | next :: remaining ->
      let safe = safe_level progression current next in
      let new_progression = get_progression current next in
      if safe then inner new_progression next remaining else false
  in
  match report with
  | first :: report -> inner Unknown first report
  | _ -> true
;;

let part_1 () =
  let reports = input () in
  let n =
    reports |> List.map ~f:evaluate_report |> List.filter ~f:Fun.id |> List.length
  in
  Printf.printf "%d\n" n
;;

let evaluate_report_adaptive report =
  let rec inner progression removed current remaining =
    match remaining with
    | [] -> true
    | next :: remaining ->
      let safe = safe_level progression current next in
      let new_progression = get_progression current next in
      if safe
      then inner new_progression removed next remaining
      else if not removed
      then inner progression true next remaining
      else false
  in
  match report with
  | first :: report -> inner Unknown false first report
  | _ -> true
;;

let part_2 () =
  let reports = input () in
  let n =
    reports
    |> List.map ~f:evaluate_report_adaptive
    |> List.filter ~f:Fun.id
    |> List.length
  in
  Printf.printf "%d\n" n
;;
