open Core

module IntTuple = struct
  type t = int * int [@@deriving sexp, compare]
end

module IntTupleSet = Set.Make (IntTuple)

let input_file = "input/8.txt"
let test_input_file = "input/8_test.txt"

let antennas_in_row y row =
  row
  |> List.filter_mapi ~f:(fun x c -> if phys_equal c '.' then None else Some (c, (x, y)))
;;

let antennas_in_map map =
  map |> List.mapi ~f:antennas_in_row |> List.fold ~init:[] ~f:( @ )
;;

let input file =
  let map = In_channel.read_lines file |> List.map ~f:String.to_list in
  let antennas = map |> antennas_in_map in
  let max_y = List.length map in
  let max_x = List.length (List.nth map 0 |> Option.value_exn) in
  max_x, max_y, antennas
;;

let vector (sx, sy) (ex, ey) = sx - ex, sy - ey

let antinode (sx, sy) (ex, ey) =
  let x_diff = sx - ex in
  let y_diff = sy - ey in
  if phys_equal x_diff 0 && phys_equal y_diff 0
  then None
  else (
    let new_x, new_y = sx + x_diff, sy + y_diff in
    printf "%d,%d -> %d,%d = %d,%d\n" sx sy ex ey new_x new_y;
    Some (new_x, new_y))
;;

let antinode_v2 max_x max_y (sx, sy) (ex, ey) =
  let x_vec = ex - sx in
  let y_vec = ey - sy in
  if phys_equal x_vec 0 && phys_equal y_vec 0
  then IntTupleSet.empty
  else
    Sequence.unfold
      ~f:(fun (x, y) ->
        let new_x = x + x_vec in
        let new_y = y + y_vec in
        if new_x >= 0 && new_y >= 0 && new_x < max_x && new_y < max_y
        then Some ((new_x, new_y), (new_x, new_y))
        else None)
      ~init:(sx, sy)
    |> IntTupleSet.of_sequence
;;

let antinodes positions =
  let rec inner current remaining antinodes =
    let new_antinodes =
      List.filter_map ~f:(antinode current) positions
      |> IntTupleSet.of_list
      |> Set.union antinodes
    in
    match remaining with
    | [] -> new_antinodes
    | next :: remaining -> inner next remaining new_antinodes
  in
  match positions with
  | current :: remaining -> inner current remaining IntTupleSet.empty
  | _ -> IntTupleSet.empty
;;

let antinodes_v2 max_x max_y positions =
  let rec inner current remaining antinodes =
    let new_antinodes =
      List.map ~f:(antinode_v2 max_x max_y current) positions
      |> List.fold ~init:IntTupleSet.empty ~f:Set.union
      |> Set.union antinodes
    in
    match remaining with
    | [] -> new_antinodes
    | next :: remaining -> inner next remaining new_antinodes
  in
  match positions with
  | current :: remaining -> inner current remaining IntTupleSet.empty
  | _ -> IntTupleSet.empty
;;

let part_1 () =
  let max_x, max_y, antennas = input input_file in
  let antenna_map = Map.of_alist_multi (module Char) antennas in
  printf "%s\n" (Map.keys antenna_map |> List.sexp_of_t Char.sexp_of_t |> Sexp.to_string);
  let antinode_positions =
    Map.data antenna_map
    |> List.map ~f:antinodes
    |> List.reduce ~f:Set.union
    |> Option.value_exn
    |> Set.filter ~f:(fun (x, y) -> x >= 0 && x < max_x && y >= 0 && y < max_y)
  in
  printf "%s\n" (IntTupleSet.sexp_of_t antinode_positions |> Sexp.to_string);
  printf "%d\n" (Set.length antinode_positions);
  ()
;;

let part_2 () =
  let max_x, max_y, antennas = input input_file in
  let antenna_map = Map.of_alist_multi (module Char) antennas in
  printf "%s\n" (Map.keys antenna_map |> List.sexp_of_t Char.sexp_of_t |> Sexp.to_string);
  let antinode_positions =
    Map.data antenna_map
    |> List.map ~f:(antinodes_v2 max_x max_y)
    |> List.reduce ~f:Set.union
    |> Option.value_exn
    |> Set.filter ~f:(fun (x, y) -> x >= 0 && x < max_x && y >= 0 && y < max_y)
  in
  printf "%s\n" (IntTupleSet.sexp_of_t antinode_positions |> Sexp.to_string);
  printf "%d\n" (Set.length antinode_positions);
  ()
;;
