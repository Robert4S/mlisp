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

let handle_call eval env func name args =
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

exception SyntaxError of string

let only_atoms exprs =
  let rec aux exprs acc =
    match exprs with
    | [] -> List.rev acc
    | Atom n :: xs -> aux xs (n :: acc)
    | _ -> raise (SyntaxError "Destructuring is not yet supported")
  in
  aux exprs []

let rec eval (env : env) (value : expr) : value =
  match value with
  | Fn (args, body) -> handle_defun args body env
  | Defun (name, args, body) ->
      let func = handle_defun args body env in
      Env.update env name ~f:(fun _ -> func);
      Env.Atom "nil"
  | List (Atom "do" :: exprs) ->
      let rec evaldo exprs =
        match exprs with
        | [ last ] -> eval env last
        | x :: xs ->
            let _ = eval env x in
            evaldo xs
        | [] -> Atom "nil"
      in
      evaldo exprs
  | List [ Atom "let"; List assignls; body ] ->
      if not (List.length assignls mod 2 = 0) then
        raise (SyntaxError "a let assign list must be of even length");
      let names =
        only_atoms @@ List.filteri assignls ~f:(fun idx _ -> idx mod 2 = 0)
      in
      let values =
        List.map ~f:(eval env)
        @@ List.filteri assignls ~f:(fun idx _ -> not (idx mod 2 = 0))
      in
      handle_userdef_call eval env Env.{ args = names; body } values
  | List (Atom name :: args) -> (
      match eval env (Atom name) with
      | Function f -> handle_call eval env (Function f) name args
      | other -> raise @@ TypeError (other, fake_func))
  | List (func :: args) -> (
      match eval env func with
      | Function f -> handle_call eval env (Function f) "" args
      | other -> raise @@ TypeError (other, fake_func))
  | Def (name, value) ->
      let value = eval env value in
      Env.update env name ~f:(fun _ -> value);
      Env.Atom "nil"
  | Int i -> Env.Int i
  | Quoted (List exprs) -> Env.List (List.map exprs ~f:(eval env))
  | Atom ":ls" -> String (Env.show env)
  | Atom name -> Env.find env name
  | String s -> Env.String s
  | ConsCell (car, cdr) -> Env.ConsCell (eval env car, eval env cdr)
  | other ->
      print_endline @@ show_expr other;
      failwith "todo"
