open Core
open Env
open Ast

let handle_defun args body (env : env) =
  Function (`Userdefined ({ args; body }, env))

let handle_userdef_call eval (env : env) (f : func) (args : value list) =
  let env = Env.push env in

  if not (List.length args = List.length f.args) then
    raise (ArgError (Function (`Userdefined (f, env)), args));

  List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
      Env.update env name ~f:(fun _ -> value));

  eval env f.body

let handle_call eval env name args =
  let func = Env.find env name in
  match func with
  | Function (`Internal f) -> (
      match name with
      | "if"
      | "cond" ->
          let args = List.map args ~f:(fun arg -> Thunk (fun () -> eval env arg)) in
          f args
      | _ ->
          let args = List.map args ~f:(eval env) in
          f args)
  | Function (`Userdefined (f, nenv)) ->
      let args = List.map args ~f:(eval env) in
      handle_userdef_call eval nenv f args
  | _ ->
      let args = List.map args ~f:(eval env) in
      raise @@ TypeError (Env.List args, func)

let rec eval (env : env) (value : expr) : value =
  match value with
  | Fn (args, body) -> handle_defun args body env
  | Defun (name, args, body) ->
      let func = handle_defun args body env in
      Env.update env name ~f:(fun _ -> func);
      Env.Atom "nil"
  | List (Atom name :: args) -> handle_call eval env name args
  | List (not_a_func :: _args) ->
      print_endline @@ show_expr not_a_func;
      raise @@ TypeError (eval env not_a_func, fake_func)
  | Def (name, value) ->
      let value = eval env value in
      Env.update env name ~f:(fun _ -> value);
      Env.Atom "nil"
  | Int i -> Env.Int i
  | Quoted (List exprs) -> Env.List (List.map exprs ~f:(eval env))
  | Atom name -> Env.find env name
  | String s -> Env.String s
  | ConsCell (car, cdr) -> Env.ConsCell (eval env car, eval env cdr)
  | other ->
      print_endline @@ show_expr other;
      failwith "todo"
