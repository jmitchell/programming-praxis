open Core.Std
open Rpn

let main () =
  match Sys.argv
        |> Array.to_list
        |> List.tl_exn
        |> String.concat ~sep:" "
        |> eval
  with
    Operand (Int x) -> print_string (Int.to_string x)
  | Operand (Float x) -> print_string (Float.to_string x)
  | _ -> failwith "Unexpected result type"

if !Sys.interactive then () else main ()
