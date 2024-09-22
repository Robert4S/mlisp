open Common_types
open Core
open Ast

let sequence_result xs =
  let rec aux xs acc =
    match xs with
    | [] -> Ok (List.rev acc)
    | Ok x :: xs -> aux xs (x :: acc)
    | Error e :: _ -> Error e
  in
  aux xs []

exception SyntaxError of string
exception KeyError of string

let rec vars (body : expr) =
  match body with
  | Atom n -> [ n ]
  | List xs -> List.(xs >>= vars)
  | ConsCell (a, b) -> vars a @ vars b
  | Quoted e -> vars e
  | ModAccess (mod_, _) -> vars mod_
  | _ -> []

and remake ?(name = None) env = Mod.remake env name

and bound_vars args body =
  let vars = ExprSet.of_list @@ List.map ~f:(fun x -> Atom x) @@ vars body in
  let args = ExprSet.of_list @@ List.map ~f:(fun x -> Atom x) args in
  Set.to_list @@ Set.diff vars args

and bound_frame args body env =
  let bound = bound_vars args body in
  let bound =
    List.(bound >>| function Atom name -> name | _ -> failwith "nuh uh bucko")
  in
  let vals =
    List.filter_map bound ~f:(fun var ->
        match Env.find env var with
        | Some value -> Some (var, value)
        | None -> None)
  in
  Env.make vals

and handle_defun args body (env : mod_t) =
  let captured = Env.to_tbl_list @@ bound_frame args body (Mod.get_env env) in
  (match Mod.name env with
  | Some n -> Env.update (Env.of_tbl_list captured) ~f:(fun _ -> Module env) n
  | _ -> ());
  Function (`Userdefined ({ args; body }, captured))

and handle_userdef_call (mod_ : mod_t) (f : func) (args : value_t list) =
  let env = Mod.get_env mod_ in
  let env = Env.push env in
  if not (List.length args = List.length f.args) then
    raise (ArgError (Function (`Userdefined (f, Env.to_tbl_list env)), args));
  List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
      Env.update env name ~f:(fun _ -> value));
  eval (Mod.remake env (Mod.name mod_)) f.body

and all predicate xs =
  match xs with [] -> true | x :: xs -> predicate x && all predicate xs

and eval_all xs =
  let rec aux xs acc =
    match xs with
    | [] -> List.rev acc
    | Thunk x :: rest -> aux rest (x () :: acc)
    | other :: rest -> aux rest (other :: acc)
  in
  aux xs []

and funcall func args =
  if all (function Thunk _ -> true | _ -> false) args then
    Thunk (fun () -> func @@ eval_all args)
  else func @@ eval_all args

and handle_call (env : mod_t) func name args =
  match func with
  | Function (`Internal f) -> (
      match name with
      | "if" | "cond" ->
          let args =
            List.map args ~f:(fun arg -> Thunk (fun () -> eval env arg))
          in
          f args
      | _ ->
          let args = List.map args ~f:(eval env) in
          funcall f args)
  | Function (`Userdefined (f, new_env)) ->
      let args = List.map args ~f:(eval env) in
      funcall
        (handle_userdef_call (Mod.remake (Env.of_tbl_list new_env) None) f)
        args
  | _ ->
      let args = List.map args ~f:(eval env) in
      raise @@ TypeError (List args, func)

and only_atoms exprs =
  let rec aux exprs acc =
    match exprs with
    | [] -> List.rev acc
    | Atom n :: xs -> aux xs (n :: acc)
    | _ -> raise (SyntaxError "Destructuring is not yet supported")
  in
  aux exprs []

and handle_map_access (env : mod_t) m field : value_t =
  match eval env m with
  | MMap m -> ( match MMap.get m field with Some v -> v | None -> Atom "nil")
  | UserDefined u -> (
      let m = Types.inner u in
      match MMap.get m field with Some v -> v | None -> Atom "nil")
  | _ -> raise @@ SyntaxError "temporary problem"

and define_map (env : mod_t) args : (value_t, string) Result.t =
  let pairs =
    List.chunks_of args ~length:2
    |> List.map ~f:(function
         (* make map keys be any value, not just atoms *)
         | [ a; b ] -> (Value.show @@ eval env a, b)
         | _ -> assert false)
    |> List.map ~f:(fun (key, value) -> (key, eval env value))
  in
  Result.(MMap.create pairs >>= fun pairs -> Ok (MMap pairs))

and extract_functions (e : expr list) =
  List.map e ~f:(function
    | List [ Atom "defun"; Atom name; List args ] ->
        Ok { name; args = List.length args }
    | _ -> Error "Only function declarations are allowed in trait bodies")
  |> sequence_result

and eval_type_create mod_ name exprs =
  let name = String.drop_prefix name 1 in
  let res =
    let open Result.Let_syntax in
    match Env.find (Mod.get_env mod_) name with
    | Some (Module assoc_mod) -> (
        let%bind type_ =
          Result.of_option ~error:"module has no associated type"
          @@ Mod.get_t assoc_mod
        in
        match eval mod_ (Ast.Map exprs) with
        | MMap m ->
            Result.of_option ~error:"cannot construct struct"
            @@ Types.construct type_ m
        | _ -> Error "value after name should be brackets")
    | _ -> Error "no module of that name"
  in
  match res with Ok v -> UserDefined v | Error e -> failwith e

and eval_deftrait mod_ name es =
  match extract_functions es with
  | Ok fs ->
      let trait = Trait (Trait.make_trait name fs) in
      Env.update (Mod.get_env mod_) name ~f:(fun _ -> trait);
      trait
  | Error e -> failwith e

and eval_defimpl mod_ trait_name mod_name body : value_t =
  let m_env = Mod.get_env mod_ in
  match Env.find m_env trait_name with
  | Some (Trait t) ->
      let m = Mod.create ~parent:(Some mod_) eval body in
      Trait.add_implementer t mod_name m;
      Atom "nil"
  | _ -> todo ()

and eval_defun mod_ name args body : value_t =
  let args = List.map ~f:(function Atom n -> n | _ -> assert false) args in
  let func = handle_defun args body mod_ in
  Mod.update mod_ name ~f:(fun _ -> func);
  (match func with
  | Function (`Userdefined (_, captured)) ->
      Env.update (Env.of_tbl_list captured) name ~f:(fun _ -> func)
  | _ -> ());
  Atom "nil"

and eval_deftype mod_ fields : value_t =
  let fields =
    List.map ~f:(function Atom a -> a | _ -> assert false) fields
  in
  let new_type =
    Types.create
      (Mod.name mod_ |> function Some x -> x | _ -> assert false)
      fields
  in
  Mod.add_type mod_ new_type;
  Atom "nil"

and eval_def mod_ name value : value_t =
  let value = eval mod_ value in
  Env.update (Mod.get_env mod_) name ~f:(fun _ -> value);
  Atom "nil"

and eval_defmod mod_ name e =
  let new_module = Mod.create ~parent:(Some mod_) ~name:(Some name) eval e in
  Mod.update mod_ ~f:(fun _ -> Module new_module) name;
  Mod.update new_module ~f:(fun _ -> Module new_module) name;
  Module new_module

and eval_mod mod_ e =
  let new_mod = Mod.create ~parent:(Some mod_) eval e in
  Module new_mod

and eval_atom_literal mod_ v : value_t =
  if String.(v = ":ls") then Fmt.pr "%s" (Env.show Value.pp (Mod.get_env mod_));
  Atom v

and eval_mod_access mod_ module_name field : value_t =
  match eval mod_ module_name with
  | Module m -> (
      match Env.find (Mod.get_env m) field with
      | Some v -> v
      | None -> Atom "nil")
  | Trait t -> (
      let module Self : sig
        val eval : mod_t -> expr -> value_t
        val handle_userdef_call : mod_t -> func -> value_t gen_func
        val bound_frame : string list -> expr -> value_t Env.t -> value_t Env.t
        val remake : ?name:string option -> value_t Env.t -> mod_t
        val funcall : value_t gen_func -> value_t list -> value_t
      end = struct
        let eval = eval
        let handle_userdef_call = handle_userdef_call
        let bound_frame = bound_frame
        let remake = remake
        let funcall = funcall
      end in
      let to_mod = Trait.to_mod (module Self) t in
      match Mod.find to_mod field with Some v -> v | None -> Atom "nil")
  | _ -> failwith "not a module"

and eval_fun mod_ args body =
  let args = List.map ~f:(function Atom x -> x | _ -> assert false) args in
  handle_defun args body mod_

and eval_map mod_ pairs =
  match define_map mod_ pairs with
  | Ok res -> res
  | Error key -> raise @@ KeyError key

and eval_import mod_ imported =
  let new_module = Module (Mod.with_file eval imported) in
  Env.update (Mod.get_env mod_) imported ~f:(fun _ -> new_module);
  new_module

and eval_do mod_ exprs =
  let rec evaldo exprs =
    match exprs with
    | [ last ] -> eval mod_ last
    | x :: xs ->
        let _ = eval mod_ x in
        evaldo xs
    | [] -> Atom "nil"
  in
  evaldo exprs

and eval_let mod_ assignls body =
  if not (List.length assignls mod 2 = 0) then
    raise (SyntaxError "a let assign list must be of even length");
  let names =
    only_atoms @@ List.filteri assignls ~f:(fun idx _ -> idx mod 2 = 0)
  in
  let values =
    List.map ~f:(eval mod_)
    @@ List.filteri assignls ~f:(fun idx _ -> not (idx mod 2 = 0))
  in
  handle_userdef_call mod_ { args = names; body } values

and eval_named_call mod_ name args =
  match eval mod_ (Atom name) with
  | Function f -> handle_call mod_ (Function f) name args
  | other -> raise @@ TypeError (other, unreachable ())

and eval_call mod_ func args =
  match eval mod_ func with
  | Function f -> handle_call mod_ (Function f) "" args
  | other -> raise @@ TypeError (other, unreachable ())

and eval : mod_t -> expr -> value_t =
 fun mod_ value ->
  match value with
  | TypeCreate (name, exprs) -> eval_type_create mod_ name exprs
  | List [ Atom "deftrait"; Atom name; List es ] -> eval_deftrait mod_ name es
  | List [ Atom "defimpl"; Atom trait_name; Atom mod_name; body ] ->
      eval_defimpl mod_ trait_name mod_name body
  | List [ Atom "defun"; Atom name; List args; body ] ->
      eval_defun mod_ name args body
  | List [ Atom "deftype"; Map fields ] -> eval_deftype mod_ fields
  | List [ Atom "def"; Atom name; value ] -> eval_def mod_ name value
  | List [ Atom "defmod"; Atom name; e ] -> eval_defmod mod_ name e
  | List [ Atom "mod"; e ] -> eval_mod mod_ e
  | Atom v when Char.(':' = (String.to_array v).(0)) -> eval_atom_literal mod_ v
  | ModAccess (module_name, field) -> eval_mod_access mod_ module_name field
  | List [ Atom "fun"; List args; body ] -> eval_fun mod_ args body
  | Map pairs -> eval_map mod_ pairs
  | List [ Atom "import"; Atom imported ] -> eval_import mod_ imported
  | List (Atom "do" :: exprs) -> eval_do mod_ exprs
  | List [ Atom "let"; List assignls; body ] -> eval_let mod_ assignls body
  (*     | List [ Atom "open"; Atom imported ] -> eval_open mod_ imported *)
  | List [ Atom "get"; field; m ] ->
      handle_map_access mod_ m (Value.show @@ eval mod_ field)
  | List [ Atom n; e ] when Char.equal (String.to_array n).(0) ':' ->
      handle_map_access mod_ e n
  | List (Atom name :: args) -> eval_named_call mod_ name args
  | List (func :: args) -> eval_call mod_ func args
  | Int i -> Int i
  | Float f -> Float f
  | Quoted (List exprs) -> List (List.map exprs ~f:(eval mod_))
  | Atom name -> Env.find_exn (Mod.get_env mod_) name
  | String s -> String s
  | ConsCell (car, cdr) -> ConsCell (eval mod_ car, eval mod_ cdr)
  | List [] -> List []
  | Quoted e -> Thunk (fun _ -> eval mod_ e)
  | Eof -> Atom "nil"
  | Set ys -> Set (List.map ~f:(eval mod_) ys)

and unreachable () = Function (`Internal (fun _ -> failwith "unreachable"))
