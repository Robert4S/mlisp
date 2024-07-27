open Core
open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | None -> Ast.Eof
  | Some o -> o

let eval_put oc env s =
  let parsed = parse s in
  match parsed with
  | List exprs ->
      let evaluated = List.map exprs ~f:(Eval.eval env) |> List.rev in
      Printf.fprintf oc "%s\n" @@ Env.show_value @@ List.hd_exn evaluated
  | _ -> ()

let evaluate_program env (s : string) = eval_put Out_channel.stdout env s

let get_text oc filename () =
  let text = In_channel.read_all filename in
  let env = Potentially.populate () in
  eval_put oc env text;
  env
