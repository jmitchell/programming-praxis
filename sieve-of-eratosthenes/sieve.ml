open Core.Std

let sieve n =
  let is_prime_array = Array.create ~len:((n-1)/2) true in
  let num_to_index x = (x/2)-1 in
  let index_to_num i = 2*(i+1)+1 in

  let sift_cap = n |> float |> sqrt |> Int.of_float in
  let rec sift_multiples_of x =
    if x > sift_cap
    then ()
    else
      let starting_point = x*x in
      let rec loop i =
        let num = starting_point + i*x in
        let index = num_to_index num in
        if index < Array.length is_prime_array
        then
          begin
            is_prime_array.(num_to_index num) <- false;
            loop (i+2);
          end
        else ();
      in
      loop 0;
      sift_multiples_of (x+2)
  in
  sift_multiples_of 3;

  if n < 2
  then []
  else 2::(is_prime_array
           |> Array.to_list
           |> List.filter_mapi ~f:(fun i b -> if b then Some (index_to_num i) else None))
