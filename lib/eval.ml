open Core
open Env
open Ast

exception SyntaxError of string
exception KeyError of string

let rec vars (body : expr) =
  match body with
  | Atom n -> [ n ]
  | List xs -> List.(xs >>= vars)
  | ConsCell (a, b) -> vars a @ vars b
  | Fn (_, e) | Defun (_, _, e) | Def (_, e) | Quoted e -> vars e
  | _ -> []

and bound_vars args body =
  let vars = ExprSet.of_list @@ List.map ~f:(fun x -> Atom x) @@ vars body in
  let args = ExprSet.of_list @@ List.map ~f:(fun x -> Atom x) args in
  Set.to_list @@ Set.diff vars args

and bound_frame args body env =
  let bound = bound_vars args body in
  let bound =
    List.(
      bound
      >>| function
      | Atom name -> name
      | _ -> failwith "nuh uh bucko")
  in
  let vals =
    List.filter_map bound ~f:(fun var ->
      match Env.find env var with
      | Some value -> Some (var, value)
      | None -> None)
  in
  Env.make vals

and handle_defun args body (env : env) =
  let captured = bound_frame args body env in
  Function (`Userdefined ({ args; body }, captured))

and handle_userdef_call (env : env) (f : func) (args : value list) =
  let env = Env.push env in
  if not (List.length args = List.length f.args)
  then raise (ArgError (Function (`Userdefined (f, env)), args));
  List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
    Env.update env name ~f:(fun _ -> value));
  eval env f.body

and handle_call env func name args =
  match func with
  | Function (`Internal f) ->
    (match name with
     | "if" | "cond" ->
       let args = List.map args ~f:(fun arg -> Thunk (fun () -> eval env arg)) in
       f args
     | _ ->
       let args = List.map args ~f:(eval env) in
       f args)
  | Function (`Userdefined (f, new_env)) ->
    let args = List.map args ~f:(eval env) in
    handle_userdef_call new_env f args
  | _ ->
    let args = List.map args ~f:(eval env) in
    raise @@ TypeError (Env.List args, func)

and only_atoms exprs =
  let rec aux exprs acc =
    match exprs with
    | [] -> List.rev acc
    | Atom n :: xs -> aux xs (n :: acc)
    | _ -> raise (SyntaxError "Destructuring is not yet supported")
  in
  aux exprs []

and handle_map_access env m field =
  match eval env m with
  | Env.Map m ->
    (match MlispMap.get m field with
     | Some v -> v
     | None -> Atom "nil")
  | _ -> raise @@ SyntaxError "temporary problem"

and define_map env args =
  let pairs =
    List.chunks_of args ~length:2
    |> List.map ~f:(function
      | [ Atom a; b ] -> a, b
      | _ -> assert false)
    |> List.map ~f:(fun (key, value) -> key, eval env value)
  in
  Result.(MlispMap.create pairs >>= fun pairs -> Ok (Env.Map pairs))

and eval (env : env) (value : expr) : value =
  match value with
  | Fn (args, body) -> handle_defun args body env
  | Defun (name, args, body) ->
    let func = handle_defun args body env in
    Env.update env name ~f:(fun _ -> func);
    (match func with
     | Function (`Userdefined (_, captured)) ->
       Env.update captured name ~f:(fun _ -> func)
     | _ -> ());
    Env.Atom "nil"
  | Map pairs ->
    (match define_map env pairs with
     | Ok res -> res
     | Error key -> raise @@ KeyError key)
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
    if not (List.length assignls mod 2 = 0)
    then raise (SyntaxError "a let assign list must be of even length");
    let names = only_atoms @@ List.filteri assignls ~f:(fun idx _ -> idx mod 2 = 0) in
    let values =
      List.map ~f:(eval env)
      @@ List.filteri assignls ~f:(fun idx _ -> not (idx mod 2 = 0))
    in
    handle_userdef_call env Env.{ args = names; body } values
  | List [ Atom field; m ] when Char.equal (String.to_array field).(0) ':' ->
    handle_map_access env m field
  | List (Atom name :: args) ->
    (match eval env (Atom name) with
     | Function f -> handle_call env (Function f) name args
     | other -> raise @@ TypeError (other, fake_func))
  | List (func :: args) ->
    (match eval env func with
     | Function f -> handle_call env (Function f) "" args
     | other -> raise @@ TypeError (other, fake_func))
  | Def (name, value) ->
    let value = eval env value in
    Env.update env name ~f:(fun _ -> value);
    Env.Atom "nil"
  | Int i -> Env.Int i
  | Float f -> Env.Float f
  | Quoted (List exprs) -> Env.List (List.map exprs ~f:(eval env))
  | Atom ":ls" -> String (Env.show env)
  | Atom name -> Env.find_exn env name
  | String s -> Env.String s
  | ConsCell (car, cdr) -> Env.ConsCell (eval env car, eval env cdr)
  | List [] -> Env.List []
  | Quoted e -> Env.Thunk (fun _ -> eval env e)
  | Eof -> Atom "nil"
;;
