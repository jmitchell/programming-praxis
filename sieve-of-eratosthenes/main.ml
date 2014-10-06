open Core.Std
open Sieve

let main () =
  let n = Int.of_string Sys.argv.(1) in
  let primes = List.map ~f:(Int.to_string) (sieve n) in
  print_string (String.concat ~sep:" " primes);
  print_newline ()

if !Sys.interactive then () else main ()
