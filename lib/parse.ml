open Core
open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | None -> Ast.Eof
  | Some o -> o

let evaluate_program (s : string) =
  let parsed = parse s in
  let env = Env.populate () in
  match parsed with
  | List exprs ->
      let evaluated = List.map exprs ~f:(Eval.eval env) |> List.rev in
      print_endline @@ Env.show_value @@ List.hd_exn evaluated
  | _ -> ()

let get_text () =
  let text = In_channel.read_all "./test.mlisp" in
  evaluate_program text
