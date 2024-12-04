open Core
open Option.Monad_infix

module Position = struct
  type t = (int * int) * (int * int) [@@deriving sexp, compare]
end

module PositionSet = Set.Make (Position)

let input_file = "input/4.txt"
let words = [ "xmas"; "samx" ]

let get arr (x, y) =
  if y < 0 || y >= Array.length arr
  then None
  else (
    let row = Array.get arr y in
    if x < 0 || x >= Array.length row then None else Some (Array.get row x))
;;

let input () =
  In_channel.read_lines input_file |> List.map ~f:String.to_array |> Array.of_list
;;

let ( >> ) f g x = g (f x)
let left (x, y) = x - 1, y
let right (x, y) = x + 1, y
let up (x, y) = x, y - 1
let down (x, y) = x, y + 1
let left_up = left >> up
let left_down = left >> down
let right_up = right >> up
let right_down = right >> down

let scan board word seen direction start =
  let rec inner i ((x, y) as current) =
    if i >= String.length word || i < 0
    then seen
    else (
      let current_char = get board current in
      let is_equal = current_char |> Option.exists ~f:(phys_equal (String.get word i)) in
      match is_equal, i with
      | true, _ when phys_equal i (String.length word - 1) ->
        if Set.mem seen (start, current) || Set.mem seen (current, start)
        then seen
        else (
          let entry = start, current in
          Set.add seen entry)
      | true, _ -> inner (i + 1) (direction (x, y))
      | false, _ -> seen)
  in
  inner 0 start
;;

let scan_radial board word seen pos =
  let s d seen = scan board word seen d pos in
  seen
  |> s up
  |> s down
  |> s left
  |> s right
  |> s left_up
  |> s left_down
  |> s right_up
  |> s right_down
;;

let find_word board word =
  let scan = scan_radial board word in
  let rec inner pos seen =
    let new_seen = scan seen pos in
    match pos with
    | _, y when y >= Array.length board -> new_seen
    | x, y when x >= Array.length (Array.get board y) -> inner (0, y + 1) new_seen
    | x, y -> inner (x + 1, y) new_seen
  in
  inner (0, 0) PositionSet.empty
;;

let part_1 () =
  let board = input () in
  let scan_for_xmas = find_word board "XMAS" in
  printf "%d\n" (Set.length scan_for_xmas);
  ()
;;

let is_x_mas board pos =
  let is_a = get board pos >>| phys_equal 'A' |> Option.value ~default:false in
  let chars =
    [ get board (left_up pos)
    ; get board (right_down pos)
    ; get board (right_up pos)
    ; get board (left_down pos)
    ]
    |> List.filter_map ~f:Fun.id
    |> List.filter ~f:(fun c -> phys_equal c 'M' || phys_equal c 'S')
  in
  if List.length chars < 4 || not is_a
  then false
  else (
    match chars with
    | [ a; b; c; d ] when phys_equal a b || phys_equal c d -> false
    | _ ->
      let x = List.sort ~compare:Char.compare chars |> String.of_list in
      String.equal x "MMSS")
;;

let positions board =
  let rec inner pos result =
    match pos with
    | _, y when y >= Array.length board -> result
    | x, y when x >= Array.length (Array.get board y) -> inner (0, y + 1) result
    | x, y -> inner (x + 1, y) (pos :: result)
  in
  inner (0, 0) []
;;

let find_x_mas board = board |> positions |> List.filter ~f:(is_x_mas board)

let part_2 () =
  let board = input () in
  let n_valid = find_x_mas board |> List.length in
  printf "%d\n" n_valid;
  ()
;;
