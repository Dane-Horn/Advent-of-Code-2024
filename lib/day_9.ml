open Core

type block =
  | File of int
  | Empty of int
[@@deriving sexp]

let input_file = "input/9.txt"
let test_input_file = "input/9_test.txt"

let input file =
  let line =
    In_channel.read_all file
    |> String.to_array
    |> Array.map ~f:(fun c -> int_of_char c - 48)
  in
  line
;;

let checksum id pos len =
  let rec inner i sum =
    if i >= len then sum else inner (i + 1) (sum + (id * (pos + i)))
  in
  inner 0 0
;;

let fill_and_get_checksum line =
  let line_len = Array.length line in
  let p_e = if phys_equal ((line_len - 1) % 2) 1 then line_len - 2 else line_len - 1 in
  let rec inner p_s p_e pos sum =
    match () with
    | _ when p_s >= line_len -> sum
    | _ when phys_equal line.(p_s) 0 -> inner (p_s + 1) p_e pos sum
    | _ when p_s < p_e && phys_equal line.(p_e) 0 -> inner p_s (p_e - 2) pos sum
    | _ when p_s >= p_e && phys_equal (p_s % 2) 1 ->
      inner (p_s + 1) p_e (pos + line.(p_s)) sum
    | _ when phys_equal (p_s % 2) 0 ->
      let id = p_s / 2 in
      let len = line.(p_s) in
      let file_sum = checksum id pos len in
      let new_pos = pos + len in
      line.(p_s) <- 0;
      inner p_s p_e new_pos (sum + file_sum)
    | _ when p_s < p_e ->
      let id = p_e / 2 in
      let len = Int.min line.(p_s) line.(p_e) in
      let file_sum = checksum id pos len in
      let new_pos = pos + len in
      line.(p_s) <- line.(p_s) - len;
      line.(p_e) <- line.(p_e) - len;
      inner p_s p_e new_pos (sum + file_sum)
    | _ -> failwith "this is weird"
  in
  inner 0 p_e 0 0
;;

let all_positions line =
  let len = Array.length line in
  let result = Array.create ~len 0 in
  let rec inner i =
    if i >= len
    then ()
    else (
      let size =
        match line.(i - 1) with
        | File size | Empty size -> size
      in
      result.(i) <- result.(i - 1) + size;
      inner (i + 1))
  in
  inner 1 |> ignore;
  result
;;

[@@@warning "-26-8"]

let fill_and_get_checksum_v2 line =
  let line_len = Array.length line in
  let positions = all_positions line in
  let p_end = if phys_equal ((line_len - 1) % 2) 1 then line_len - 2 else line_len - 1 in
  (* 00992111777.44.333....5555.6666.....8888.. *)
  let compact_files = Array.of_list [] in
  let rec inner p_s p_m p_e sum =
    if p_s >= line_len
    then sum
    else (
      match line.(p_s), line.(p_m), line.(p_e) with
      | _, File _, _ -> inner p_s (p_m + 1) p_e sum
      | _, Empty empty_size, File file_size when file_size > empty_size && p_e > 0 ->
        if p_m >= p_e then inner p_s 1 (p_e - 2) sum else inner p_s (p_m + 2) p_e sum
      | _, Empty empty_size, File file_size when p_e > 0 ->
        let id = p_e / 2 in
        let file_sum = checksum id positions.(p_m) file_size in
        line.(p_m) <- Empty (empty_size - file_size);
        line.(p_e) <- Empty file_size;
        positions.(p_m) <- positions.(p_m) + file_size;
        inner p_s 1 (p_e - 2) (sum + file_sum)
      | Empty _, _, _ -> inner (p_s + 2) p_m p_e sum
      | File size, _, _ ->
        let id = p_s / 2 in
        let file_sum = checksum id positions.(p_s) size in
        inner (p_s + 2) p_m p_e (sum + file_sum))
  in
  inner 0 0 p_end 0
;;

let part_1 () =
  let line = input input_file in
  let checksum = fill_and_get_checksum line in
  printf "%d\n" checksum;
  ()
;;

let part_2 () =
  let line = input input_file in
  let blocks =
    Array.mapi
      ~f:(fun i size -> if phys_equal (i % 2) 0 then File size else Empty size)
      line
  in
  let checksum = fill_and_get_checksum_v2 blocks in
  printf "%d\n" checksum;
  ()
;;
