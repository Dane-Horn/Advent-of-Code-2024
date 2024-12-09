[@@@warning "-26-27"]

open Core

type direction =
  | Left
  | Right
  | Up
  | Down
[@@deriving sexp, compare]

module IntTuple = struct
  type t = int * int [@@deriving sexp, compare]
end

module IntTupleSet = Set.Make (IntTuple)

module Position = struct
  type t = (int * int) * direction [@@deriving sexp, compare]
end

module MovementSet = Set.Make (Position)

let input_file = "input/6.txt"

let get_direction = function
  | Left -> fun (x, y) -> x - 1, y
  | Right -> fun (x, y) -> x + 1, y
  | Up -> fun (x, y) -> x, y - 1
  | Down -> fun (x, y) -> x, y + 1
;;

let next_direction = function
  | Left -> Up
  | Up -> Right
  | Right -> Down
  | Down -> Left
;;

let find_next map dir current =
  let dir_fn = get_direction dir in
  let next = dir_fn current in
  match next with
  | _, y when y < 0 || y >= Array.length map -> None
  | x, y when x < 0 || x >= Array.length map.(y) -> None
  | x, y when phys_equal map.(y).(x) '#' -> Some (next_direction dir, current, None)
  | _ -> Some (dir, next, Some (next, dir))
;;

let find_all map dir current =
  let seen = MovementSet.singleton (current, dir) in
  let find_next = find_next map in
  let init = dir, current in
  let rec inner seen (dir, current) =
    match find_next dir current with
    | None -> seen, false
    | Some (new_dir, next, new_pos_opt) ->
      let has_been_seen =
        match new_pos_opt with
        | Some new_pos when Set.mem seen new_pos ->
          (*printf "%s" (MovementSet.sexp_of_t seen |> Sexp.to_string);*)
          true
        | _ -> false
      in
      if has_been_seen
      then seen, true
      else (
        let new_seen = Option.value_map ~default:seen ~f:(Set.add seen) new_pos_opt in
        inner new_seen (new_dir, next))
  in
  inner seen init
;;

let find_start map =
  let target = '^' in
  let rec inner (x, y) =
    match x, y with
    | _, y when y >= Array.length map -> failwith "No starting position"
    | x, y when x >= Array.length (Array.get map y) -> inner (0, y + 1)
    | x, y when phys_equal (Array.get (Array.get map y) x) target -> x, y
    | x, y -> inner (x + 1, y)
  in
  inner (0, 0)
;;

let input () =
  let lines = In_channel.read_lines input_file |> Array.of_list_map ~f:String.to_array in
  lines
;;

let part_1 () =
  let map = input () in
  let start = find_start map in
  let positions, _ = find_all map Up start in
  printf "%d\n" (IntTupleSet.map ~f:fst positions |> Set.length);
  ()
;;

let looping_position map start dir curr =
  match find_next map dir curr with
  | Some (_, ((x, y) as next), Some _) ->
    if phys_equal next start
    then None
    else if phys_equal map.(y).(x) '.'
    then (
      map.(y).(x) <- '#';
      let positions, infinite = find_all map Up start in
      (*printf "infinite: %b\n" infinite;*)
      let pos_len = IntTupleSet.map ~f:fst positions |> Set.length in
      map.(y).(x) <- '.';
      if infinite
      then (
        if pos_len < 10
        then printf "%d: %d,%d %s\n" pos_len x y (sexp_of_direction dir |> Sexp.to_string);
        Some next)
      else None)
    else None
  | _ -> None
;;

let part_2 () =
  let map = input () in
  let start = find_start map in
  let positions, _ = find_all map Up start in
  let infinite_positions =
    positions
    |> Set.to_list
    |> List.filter_map ~f:(fun (pos, dir) -> looping_position map start dir pos)
    |> IntTupleSet.of_list
  in
  printf "%d\n" (Set.length infinite_positions);
  ()
;;
