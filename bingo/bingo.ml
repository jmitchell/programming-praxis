open Core.Std

let rec shuffle = function
  | [] -> []
  | [x] -> [x]
  | xs ->
     let rand_index = Random.int (List.length xs) in
     let new_head = List.nth_exn xs rand_index in
     let new_tail = List.take xs rand_index @ List.drop xs (rand_index + 1) in
     (new_head :: shuffle new_tail)

type square = Occupied | Vacant of int
type card = square array array

let random_card () =
  let column_vals min max n =
    let possible_vals =
      List.range min (max+1) in
    let n_choosen_vals =
      List.map (List.take (shuffle possible_vals) n)
	       ~f:(fun x -> Vacant x) in
    Array.of_list n_choosen_vals in
  let middle_column =
    let vacant_vals =
      column_vals 31 45 4 in
    let col_array =
      Array.create 5 Occupied in
    Array.set col_array 0 vacant_vals.(0);
    Array.set col_array 1 vacant_vals.(1);
    Array.set col_array 3 vacant_vals.(2);
    Array.set col_array 4 vacant_vals.(3);
    col_array in
  Array.of_list [
      column_vals 1 15 5;
      column_vals 16 30 5;
      middle_column;
      column_vals 46 60 5;
      column_vals 61 75 5
    ]

let is_bingo card =
  let rec are_occupied = function
    | [] -> true
    | ((c, r) :: xs) ->
       if not (card.(c).(r) = Occupied)
       then false
       else are_occupied xs
  in
  let lines_of_squares =
    let span =
      List.range 0 5 in
    let rows =
      List.map span
               ~f:(fun r ->
                   List.map span ~f:(fun c -> (c,r))) in
    let cols =
      List.map span
               ~f:(fun c ->
                  List.map span ~f:(fun r -> (c,r))) in
    let diags =
      [
        [(0,0); (1,1); (2,2); (3,3); (4,4)];
        [(0,4); (1,3); (2,2); (3,1); (4,0)]
      ] in
    rows @ cols @ diags
  in
  List.exists lines_of_squares ~f:are_occupied

let handle_call n card =
  for i = 0 to 4 do
    for j = 0 to 4 do
      if card.(i).(j) = Vacant n
      then card.(i).(j) <- Occupied
      else ()
    done
  done

let bingo_session seats =
  let cards =
    List.map (List.range 0 seats) ~f:(fun _ -> random_card ()) in
  let any_bingos () =
    List.exists cards ~f:is_bingo in
  let random_call_sequence =
    shuffle (List.range 1 76) in
  let rec announce_number remaining_numbers call_count =
    let n =
      List.hd_exn remaining_numbers in
    List.iter cards ~f:(handle_call n);
    if any_bingos ()
    then call_count
    else announce_number (List.tl_exn remaining_numbers) (call_count + 1)
  in
  announce_number random_call_sequence 1

let average_calls_required seats simulations =
  let call_counts =
    List.map (List.range 0 simulations)
             ~f:(fun _ -> bingo_session seats) in
  let sum =
    Float.of_int (List.fold call_counts ~init:0 ~f:(+)) in
  sum /. Float.of_int simulations

let () =
  Random.self_init ()
