open Core
open Env
open Ast

let handle_defun args body = Function (`Userdefined { args; body })

let rec eval (env : env) (value : expr) =
  match value with
  | Defun (name, args, body) ->
      let func = handle_defun args body in
      Env.update env name ~f:(fun _ -> func);
      Env.Atom "nil"
  | List (Atom name :: args) ->
      let handle_userdef_call (env : env) (f : func) (args : value list) =
        let env = Env.push env in

        if not (List.length args = List.length f.args) then
          raise (ArgError (Function (`Userdefined f), args));

        List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
            Env.update env name ~f:(fun _ -> value));

        eval env f.body
      in

      let handle_call env name args =
        let func = Env.find env name in
        match func with
        | Function (`Internal f) -> (
            match name with
            | "if"
            | "cond" ->
                let args =
                  List.map args ~f:(fun arg -> Thunk (fun () -> eval env arg))
                in
                f args
            | _ ->
                let args = List.map args ~f:(eval env) in
                f args)
        | Function (`Userdefined f) ->
            let args = List.map args ~f:(eval env) in
            handle_userdef_call env f args
        | _ ->
            let args = List.map args ~f:(eval env) in
            raise @@ TypeError (Env.List args, func)
      in
      handle_call env name args
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
  | other ->
      print_endline @@ show_expr other;
      failwith "todo"
