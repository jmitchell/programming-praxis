open Core.Std
open OUnit2
open Rpn

let ae expected actual _test_ctxt = assert_equal expected actual

let tests =
  ["parse int">::
     ae [Operand (Int 5)] (parse_expression "5");
   "parse float">::
     ae [Operand (Float 2.14)] (parse_expression "2.14");
   "parse addition expression">::
     ae [Operand (Int 2); Operand (Int 3); BinOp Add] (parse_expression "2 3 +");
   "eval int">::
     ae (Operand (Int 5)) (eval "5");
   "eval addition expression">::
     ae (Operand (Int 5)) (eval "2 3 +");
   "eval subtraction expression">::
     ae (Operand (Int (-2))) (eval "3 5 -")
  ]

let () =
  run_test_tt_main ("rpn tests" >::: tests)
