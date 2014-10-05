open Core.Std

type binary_operator = Add | Subtract | Multiply | Divide
type operand = Int of int | Float of float
type rpn_element = BinOp of binary_operator | Operand of operand

let parse_expression expr =
  let element x =
    let binop = function
        "+" -> Some (BinOp Add) | "-" -> Some (BinOp Subtract) |
        "*" -> Some (BinOp Multiply) | "/" -> Some (BinOp Divide) |
        _ -> None
    in
    let operand n =
      try Some (Operand (Int (Int.of_string n)))
      with _ ->
        try Some (Operand (Float (Float.of_string n)))
        with _ -> None
    in
    match operand x with
      Some x' -> x'
    | None ->
       match binop x with
         Some x' -> x'
       | None -> failwith "Unexpected operator or operand"
  in
  List.map ~f:element (String.split ~on:' ' expr)

let eval expr =
  let apply op (Operand a) (Operand b) =
    let int_op =
      match op with
        Add -> (+) | Subtract -> (-) | Multiply -> ( * ) | Divide -> (/) |
        _ -> failwith "Unexpected operator"
    in
    let float_op =
      match op with
        Add -> (+.) | Subtract -> (-.) | Multiply -> ( *. ) | Divide -> (/.) |
        _ -> failwith "Unexpected operator"
    in
    Operand (
        match (a, b) with
          (Int a', Int b') -> Int (int_op a' b')
        | (Int a', Float b') -> Float (float_op (float a') b')
        | (Float a', Int b') -> Float (float_op a' (float b'))
        | (Float a', Float b') -> Float (float_op a' b')
        | _ -> failwith "Unexpected operand types")
  in
  let rec eval_stack stack queue =
    match queue with
      [] ->
      (match List.hd stack with
         Some h -> h
       | None -> failwith "No result; stack is empty")
    | x::xs ->
       match x with
         Operand _ -> eval_stack (x::stack) xs
       | BinOp op ->
          match stack with
            [] | [_] -> failwith "Too few operands"
            | y1::y2::ys -> eval_stack ((apply op y2 y1)::ys) xs
  in
  eval_stack [] (parse_expression expr)
