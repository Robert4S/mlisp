open Core
open Env

let eval_function func env = Eval.handle_userdef_call Eval.eval env func []

let rec hd_ (vals : Env.value list) =
  match vals with
  | [ Env.List (x :: _) ] -> x
  | [ List [] ] -> Atom "nil"
  | [ ConsCell (car, _cdr) ] -> car
  | [ String s ] -> String (Char.to_string @@ String.get s 0)
  | [ Function (`Userdefined (func, env)) ] -> hd_ @@ [ eval_function func env ]
  | _ -> raise (InvalidArg "cannot take the head of something that is not a collection")
;;

let rec tail vals =
  match vals with
  | [ List (_ :: xs) ] -> List xs
  | [ List [] ] -> Atom "nil"
  | [ ConsCell (_car, cdr) ] -> cdr
  | [ Function (`Userdefined (func, env)) ] -> tail @@ [ eval_function func env ]
  | _ -> raise (InvalidArg "tail can only take the tail of a single list")
;;

let quit _vals =
  print_endline "Goodbye!";
  exit 0
;;

let typeof vals : value =
  let rec aux vals acc : string =
    match vals with
    | [] -> acc
    | x :: xs ->
      let curr =
        match x with
        | String _ -> "String "
        | Atom a -> sprintf "Atom %s " a
        | Function _ -> "Function "
        | Int _ -> "Int "
        | Float _ -> "Float "
        | Thunk _ -> "Thunk "
        | List _ -> "List "
        | ConsCell (a, b) -> sprintf "ConsCell (%s) (%s) " (aux [ a ] "") (aux [ b ] "")
        | Map _ -> "Map "
      in
      let acc = String.append acc curr in
      aux xs acc
  in
  String (aux vals "")
;;

let populate () =
  let items =
    Env.
      [ "true", Atom "true"
      ; ":else", Atom "true"
      ; "false", Atom "false"
      ; "nil", Atom "nil"
      ; "+", Function (`Internal add)
      ; "-", Function (`Internal sub)
      ; "*", Function (`Internal mul)
      ; "/", Function (`Internal div)
      ; ">", Function (`Internal gt)
      ; "<", Function (`Internal lt)
      ; ">=", Function (`Internal gte)
      ; "<=", Function (`Internal lte)
      ; "=", Function (`Internal eq)
      ; "mod", Function (`Internal mod_)
      ; "if", Function (`Internal if_)
      ; "str", Function (`Internal str)
      ; "io-puts", Function (`Internal io_puts)
      ; "cond", Function (`Internal cond)
      ; "tail", Function (`Internal tail)
      ; "cons", Function (`Internal cons)
      ; "car", Function (`Internal car)
      ; "cdr", Function (`Internal cdr)
      ; "hd", Function (`Internal hd_)
      ; "keys", Function (`Internal keys)
      ; "values", Function (`Internal values)
      ; "pairs", Function (`Internal pairs)
      ; "quit", Function (`Internal quit)
      ; "type", Function (`Internal typeof)
      ; "nil?", Function (`Internal nil)
      ; "list?", Function (`Internal list)
      ; "fun?", Function (`Internal fun_)
      ; "cell?", Function (`Internal cell)
      ]
  in
  let tbl =
    [ Hashtbl.create
        ~growth_allowed:true
        ~size:(int_of_float (float_of_int (List.length items) *. 1.5))
        (module String)
    ]
  in
  mass_add tbl items;
  tbl
;;
