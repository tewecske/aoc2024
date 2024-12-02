(* let () = print_endline "Hello, OCaml World!" *)
(* let () = Printf.printf "%s\n" Aoc2024ocaml.En.v *)

(* Read all lines from a file into a string list *)
let read_lines filename =
  let ic = open_in filename in
  let rec read_lines_aux acc =
    try
      let line = input_line ic in
      read_lines_aux (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines_aux []

let file_contents = read_lines "day1_input"
let first_element = List.hd file_contents

let convert_string_pairs input_list =
 let split_and_convert str =
   match str
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
        with
   | [first; second] -> 
       (int_of_string first, int_of_string second)
   | _ -> failwith "Invalid string format"
 in
 
 let pairs = List.map split_and_convert input_list in
 
 let first_list = List.map fst pairs in
 let second_list = List.map snd pairs in
 
 (first_list, second_list)

let first_list, second_list = convert_string_pairs file_contents

let first_list_sorted = List.sort Int.compare first_list
let second_list_sorted = List.sort Int.compare second_list

let result =
    List.combine first_list_sorted second_list_sorted
    |> List.map (fun (first, second) -> abs (first - second))
    |> List.fold_left (+) 0

(* let similarity_result = *)
(*   let find_similar list x = *)
(*     List.filter ((=) x) list *)
(*     |> List.length *)
(*   in *)
(**)
(*   let rrr = List.map (fun e -> find_similar second_list_sorted e) first_list_sorted *)
(*   rrr *)

let count_occurrences list1 list2 =
  list1
  |> List.map (fun x ->
       x * (list2 |> List.filter ((=) x) |> List.length))

let sum list =
  list
  |> List.fold_left (+) 0

let similarity_result = count_occurrences first_list_sorted second_list_sorted
    |> sum

let () =
    Printf.printf "First element %s\n" first_element;
    Printf.printf "First splitted %d %d\n" (List.hd first_list) (List.hd second_list);
    Printf.printf "First sorted %d %d\n" (List.hd first_list_sorted) (List.hd second_list_sorted);
    Printf.printf "Result: %d\n" result;
    Printf.printf "Similarity result: %d\n" similarity_result

