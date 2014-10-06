open Core.Std
open OUnit2
open Sieve

let ae expected actual _test_ctxt = assert_equal expected actual

let tests =
  [
    "given example">::
      ae [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] (sieve 30);
    "pi(100000) = 78498">::
      ae 78498 (List.length (sieve 1000000))
  ]

let () =
  run_test_tt_main ("rpn tests" >::: tests)
