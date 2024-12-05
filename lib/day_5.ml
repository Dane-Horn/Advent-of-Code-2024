open Core
open Angstrom

let input_file = "input/5.txt"

let p_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let p_new_line = char '\n'

let p_rule =
  let* pre = p_integer in
  let* _ = char '|' in
  let* post = p_integer in
  let* _ = p_new_line in
  return (pre, post)
;;

let p_sep = p_new_line
let p_rules = many p_rule
let p_update = sep_by (char ',') p_integer <* p_new_line
let p_updates = many p_update

let p_file =
  let* rules = p_rules in
  let* _ = p_sep in
  let* updates = p_updates in
  return (rules, updates)
;;

let input () =
  let text = In_channel.read_all input_file in
  parse_string ~consume:Prefix p_file text |> Result.ok_or_failwith
;;

let rules_of_pairs pairs =
  let add_to_set_if_exists v set =
    let set_option = Option.map ~f:(fun set -> Set.add set v) set in
    Option.value ~default:(Set.of_list (module Int) [ v ]) set_option
  in
  let rec loop remaining mapping =
    match remaining with
    | [] -> mapping
    | (pre, post) :: remaining ->
      let new_mapping = Map.update ~f:(add_to_set_if_exists pre) mapping post in
      loop remaining new_mapping
  in
  loop pairs (Map.empty (module Int))
;;

let print_int_set set =
  Set.iter ~f:(printf "%d, ") set;
  printf "\n"
;;

let verify_update rules updates =
  (*List.to_string ~f:Int.to_string updates |> printf "%s";*)
  let update_set = Set.of_list (module Int) updates in
  let is_valid seen value =
    if Map.mem rules value
    then (
      let pre_reqs = Map.find rules value |> Option.value_exn |> Set.inter update_set in
      let diff = Set.diff pre_reqs seen in
      Set.length diff |> ( = ) 0)
    else true
  in
  let rec inner seen remaining =
    match remaining with
    | [] -> true
    | next :: remaining ->
      let new_seen = Set.add seen next in
      if is_valid new_seen next then inner new_seen remaining else false
  in
  inner (Set.empty (module Int)) updates
;;

let middle_value lst =
  let len = List.length lst in
  let mid = len / 2 in
  List.nth lst mid |> Option.value_exn
;;

let part_1 () =
  let pairs, updates = input () in
  let rules = rules_of_pairs pairs in
  (*Map.iteri*)
  (*  ~f:(fun ~key ~data ->*)
  (*    let data_string = data |> Set.to_list |> List.to_string ~f:Int.to_string in*)
  (*    printf "%d: %s\n" key data_string)*)
  (*  rules;*)
  let valid_updates = List.filter ~f:(verify_update rules) updates in
  let sum =
    valid_updates |> List.map ~f:middle_value |> List.sum (module Int) ~f:Fun.id
  in
  printf "%d\n" sum;
  (*List.iter ~f:(fun update -> verify_update rules update |> printf "%b\n") updates;*)
  ()
;;

let ( >> ) f g x = g (f x)

let reorder_update rules update =
  let update_set = Set.of_list (module Int) update in
  let pre_reqs value =
    Map.find rules value
    |> Option.value ~default:(Set.empty (module Int))
    |> Set.inter update_set
  in
  List.sort
    ~compare:(fun a b ->
      Int.compare (pre_reqs a |> Set.length) (pre_reqs b |> Set.length))
    update
;;

let part_2 () =
  let pairs, updates = input () in
  let rules = rules_of_pairs pairs in
  let invalid_updates = List.filter ~f:(verify_update rules >> not) updates in
  invalid_updates
  |> List.map ~f:(reorder_update rules)
  |> List.iter ~f:(fun update -> List.to_string ~f:Int.to_string update |> printf "%s\n");
  let sum =
    invalid_updates
    |> List.map ~f:(reorder_update rules >> middle_value)
    |> List.sum (module Int) ~f:Fun.id
  in
  printf "%d\n" sum;
  ()
;;
