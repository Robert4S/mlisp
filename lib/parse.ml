open Core
open Ast
open Common_types

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with None -> Ast.Eof | Some o -> o

let eval_put (eval : eval) oc env s =
  let parsed = parse s in
  match parsed with
  | List exprs ->
      let evaluated = List.map exprs ~f:(eval env) |> List.rev in
      Printf.fprintf oc "%s\n" @@ Value.show @@ List.hd_exn evaluated
  | _ -> ()

let evaluate_program eval (env : mod_t) (s : string) =
  eval_put eval Out_channel.stdout env s

let evaluate_expr (eval : eval) (env : mod_t) (expr : expr) =
  match expr with
  | List exprs ->
      let evaluated = List.map exprs ~f:(eval env) |> List.rev in
      Printf.fprintf Out_channel.stdout "%s\n"
      @@ Value.show @@ List.hd_exn evaluated
  | _ -> ()
