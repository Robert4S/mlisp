open Core
open MMap
open Ast
open Common_types
module F = Fmt

let todo _ = failwith "todo"

let sequence_result xs =
  let rec aux xs acc =
    match xs with
    | [] -> Ok (List.rev acc)
    | Ok x :: xs -> aux xs (x :: acc)
    | Error e :: _ -> Error e
  in
  aux xs []

module rec Builtins : sig
  val items : unit -> (string * Value.t) list
end =
  Prelude.Builtins (Mod) (MlispMap) (Value) (Eval)

and Value : (sig
  type userdef_t
  type trait_t

  type t =
    | Int of int
    | Float of float
    | Atom of string
    | Function of [ `Userdefined of func * t gen_hashtable | `Internal of t gen_func ]
    | String of string
    | List of t list
    | Thunk of t delayed
    | ConsCell of t cons_cell
    | Map of t MlispMap.t
    | Ref of t ref
    | Set of t list
    | Module of Mod.t
    | UserDefined of userdef_t
    | Trait of trait_t
  [@@deriving eq, ord, show, sexp]

  exception TypeError of t * t
  exception ArgError of t * t list

  val typeof : t list -> t
end
with type userdef_t = Types.value
 and type trait_t = Trait.t) = struct
  type userdef_t = Types.value
  type trait_t = Trait.t

  type t =
    | Int of int
    | Float of float
    | Atom of string
    | Function of [ `Userdefined of func * t gen_hashtable | `Internal of t gen_func ]
    | String of string
    | List of t list
    | Thunk of t delayed
    | ConsCell of t cons_cell
    | Map of t MlispMap.t
    | Ref of t ref
    | Set of t list
    | Module of Mod.t
    | UserDefined of Types.value
    | Trait of Trait.t
  [@@deriving eq, ord]

  exception TypeError of t * t
  exception ArgError of t * t list

  let rec pp (formatter : Format.formatter) value =
    match value with
    | Int i -> Format.fprintf formatter "%d" i
    | Float f -> Format.fprintf formatter "%f" f
    | Atom a -> Format.fprintf formatter "%s" a
    | Function _ -> Format.fprintf formatter "<func>"
    | String s -> Format.fprintf formatter "\"%s\"" s
    | List vals ->
        List.iter vals ~f:(fun value -> Format.fprintf formatter "%a " pp value)
    | Thunk _ -> Format.fprintf formatter "<thunk>"
    | ConsCell (car, cdr) -> Format.fprintf formatter "%a . %a" pp car pp cdr
    | Map m -> MlispMap.pp pp formatter m
    | Ref x -> Format.fprintf formatter "Ref(%a)" pp !x
    | Set xs ->
        Format.fprintf formatter "#{";
        (match xs with
        | first :: rest ->
            Format.fprintf formatter "%a" pp first;
            List.iter rest ~f:(fun value -> Format.fprintf formatter " %a" pp value)
        | _ -> ());
        Format.fprintf formatter "}"
    | Module _ -> Format.fprintf formatter "<Module>"
    | UserDefined u ->
        let name = Types.parent @@ Types.get_type u in
        let inner = Types.inner u in
        Format.fprintf formatter "#%s" name;
        MlispMap.pp pp formatter inner
    | Trait _ -> Format.fprintf formatter "<Trait>"

  and typeof (vals : Value.t list) : Value.t =
    let rec aux vals acc : string =
      match vals with
      | [] -> acc
      | x :: xs ->
          let curr =
            match x with
            | Value.String _ -> "String "
            | Atom a -> sprintf "Atom %s " a
            | Function _ -> "Function "
            | Int _ -> "Int "
            | Float _ -> "Float "
            | Thunk _ -> "Thunk "
            | List _ -> "List "
            | ConsCell (a, b) ->
                sprintf "ConsCell (%s) (%s) " (aux [ a ] "") (aux [ b ] "")
            | Map _ -> "Map "
            | Ref r ->
                "Ref "
                ^ (function
                    | String s -> s
                    | _ -> assert false)
                @@ typeof [ !r ]
            | Set _ -> "Set "
            | Module _ -> "Module "
            | UserDefined x -> Types.parent (Types.get_type x) ^ " "
            | Trait _ -> "Trait "
          in
          let acc = String.append acc curr in
          aux xs acc
    in
    String (aux vals "")

  let show value = Format.asprintf "%a" pp value
  let sexp_of_t = sexp_of_opaque
  let t_of_sexp = opaque_of_sexp
end

and Mod : (sig
  type env_t
  type t [@@deriving eq, ord, show]

  val with_file : string -> t
  val create : ?name:string option -> ?parent:Mod.t option -> expr -> t
  val get_env : t -> env_t
  val name : t -> string option
  val remake : ?t:Types.t option -> env_t -> string option -> t
  val add_type : t -> Types.t -> unit
  val get_t : t -> Types.t option
  val make_new : string option -> t
end
with type env_t = Value.t Env.t) = struct
  type t = { env : Value.t Env.t; name : string option; mutable t : Types.t option }
  [@@deriving show]

  type env_t = Value.t Env.t

  let rec remake ?(t = None) env name = { env; name; t }
  and add_type x t = x.t <- Some t

  and merge m1 m2 name t =
    let env = Env.of_tbl_list [] in
    let pairs1 = Env.pairs m1.env in
    let pairs2 = Env.pairs m2.env in
    let duplicates =
      List.filter_map pairs1 ~f:(fun (key, value) ->
          match List.find pairs2 ~f:(fun (key2, _) -> String.(key2 = key)) with
          | Some (_, value2) -> Some (key, value, value2)
          | None -> None)
    in
    let duplicates =
      List.map duplicates ~f:(fun (key, val1, val2) ->
          match (val1, val2) with
          | Trait t1, Trait t2 ->
              let t2_impls =
                Trait.impls t2
                |> List.filter ~f:(fun (name, _) -> Trait.has_implementer t1 name)
              in
              List.iter t2_impls ~f:(fun (name, impl) ->
                  Trait.add_implementer t1 name impl);
              (key, Value.Trait t1)
          | other, _ -> (key, other))
    in
    let find_non_dups l1 l2 =
      List.filter l1 ~f:(fun (key, _) ->
          not (List.exists l2 ~f:(fun (key2, _) -> String.(key = key2))))
    in
    let non_dups = find_non_dups pairs1 pairs2 @ find_non_dups pairs2 pairs1 in
    Env.mass_add env (duplicates @ non_dups);
    Mod.remake ~t env name

  and make_new name =
    let env = Eval.populate () in
    remake env name

  and with_file name =
    let contents = In_channel.read_all (name ^ ".mlisp") in
    let expr = Parse.parse contents in
    create ~name:(Some name) expr

  and create ?(name = None) ?(parent = None) expr =
    let mod_ =
      {
        env =
          (match parent with
          | None -> Eval.populate ()
          | Some e -> get_env e);
        name;
        t = None;
      }
    in
    (match name with
    | Some n -> Env.update mod_.env n ~f:(fun _ -> Module mod_)
    | _ -> ());
    match expr with
    | List xs ->
        List.iter xs ~f:(fun e -> Eval.eval mod_ e |> ignore);
        mod_
    | _ -> mod_

  and get_env { env; _ } = env
  and equal _ _ = false
  and compare _ _ = Int.max_value
  and name { name; _ } = name
  and get_t { t; _ } = t
end

and Eval : sig
  val eval : Mod.t -> expr -> Value.t
  val handle_userdef_call : Mod.t -> func -> Value.t gen_func
  val bound_frame : string list -> expr -> Value.t Env.t -> Value.t Env.t
  val remake : ?name:string option -> Value.t Env.t -> Mod.t
  val funcall : Value.t gen_func -> Value.t list -> Value.t
  val populate : unit -> Value.t Env.t
end = struct
  open Core
  open Ast

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
      List.(
        bound >>| function
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

  and handle_defun args body (env : Mod.t) =
    let captured = Env.to_tbl_list @@ bound_frame args body (Mod.get_env env) in
    (match Mod.name env with
    | Some n -> Env.update (Env.of_tbl_list captured) ~f:(fun _ -> Module env) n
    | _ -> ());
    Value.Function (`Userdefined ({ args; body }, captured))

  and handle_userdef_call (mod_ : Mod.t) (f : func) (args : Value.t list) =
    let env = Mod.get_env mod_ in
    let env = Env.push env in
    if not (List.length args = List.length f.args) then
      raise (Value.ArgError (Function (`Userdefined (f, Env.to_tbl_list env)), args));
    List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
        Env.update env name ~f:(fun _ -> value));
    eval (Mod.remake env (Mod.name mod_)) f.body

  and all predicate xs =
    match xs with
    | [] -> true
    | x :: xs -> predicate x && all predicate xs

  and eval_all xs =
    let rec aux xs acc =
      match xs with
      | [] -> List.rev acc
      | Value.Thunk x :: rest -> aux rest (x () :: acc)
      | other :: rest -> aux rest (other :: acc)
    in
    aux xs []

  and funcall func args =
    if
      all
        (function
          | Value.Thunk _ -> true
          | _ -> false)
        args
    then Value.Thunk (fun () -> func @@ eval_all args)
    else func @@ eval_all args

  and handle_call (env : Mod.t) func name args =
    match func with
    | Value.Function (`Internal f) -> (
        match name with
        | "if"
        | "cond" ->
            let args =
              List.map args ~f:(fun arg -> Value.Thunk (fun () -> eval env arg))
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
        raise @@ Value.TypeError (Value.List args, func)

  and only_atoms exprs =
    let rec aux exprs acc =
      match exprs with
      | [] -> List.rev acc
      | Atom n :: xs -> aux xs (n :: acc)
      | _ -> raise (SyntaxError "Destructuring is not yet supported")
    in
    aux exprs []

  and handle_map_access (env : Mod.t) m field =
    match eval env m with
    | Value.Map m -> (
        match MlispMap.get m field with
        | Some v -> v
        | None -> Atom "nil")
    | Value.UserDefined u -> (
        let m = Types.inner u in
        match MlispMap.get m field with
        | Some v -> v
        | None -> Atom "nil")
    | _ -> raise @@ SyntaxError "temporary problem"

  and define_map (env : Mod.t) args =
    let pairs =
      List.chunks_of args ~length:2
      |> List.map ~f:(function
           (* make map keys be any value, not just atoms *)
           | [ a; b ] -> (Value.show @@ eval env a, b)
           | _ -> assert false)
      |> List.map ~f:(fun (key, value) -> (key, eval env value))
    in
    Result.(MlispMap.create pairs >>= fun pairs -> Ok (Value.Map pairs))

  and extract_functions (e : expr list) =
    List.map e ~f:(function
      | List [ Atom "defun"; Atom name; List args ] ->
          Ok (Trait.make_f name (List.length args))
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
          | Value.Map m ->
              Result.of_option ~error:"cannot construct struct"
              @@ Types.construct type_ m
          | _ -> Error "value after name should be brackets")
      | _ -> Error "no module of that name"
    in
    match res with
    | Ok v -> Value.UserDefined v
    | Error e -> failwith e

  and eval_deftrait mod_ name es =
    match extract_functions es with
    | Ok fs ->
        let trait = Value.Trait (Trait.make_trait name fs) in
        Env.update (Mod.get_env mod_) name ~f:(fun _ -> trait);
        trait
    | Error e -> failwith e

  and eval_defimpl mod_ trait_name mod_name body =
    let m_env = Mod.get_env mod_ in
    match Env.find m_env trait_name with
    | Some (Trait t) ->
        let m = Mod.create ~parent:(Some mod_) body in
        Trait.add_implementer t mod_name m;
        Value.Atom "nil"
    | _ -> todo ()

  and eval_defun mod_ name args body =
    let args =
      List.map
        ~f:(function
          | Atom n -> n
          | _ -> assert false)
        args
    in
    let func = handle_defun args body mod_ in
    Env.update (Mod.get_env mod_) name ~f:(fun _ -> func);
    (match func with
    | Function (`Userdefined (_, captured)) ->
        Env.update (Env.of_tbl_list captured) name ~f:(fun _ -> func)
    | _ -> ());
    Value.Atom "nil"

  and eval_deftype mod_ fields =
    let fields =
      List.map
        ~f:(function
          | Atom a -> a
          | _ -> assert false)
        fields
    in
    let new_type =
      Types.create
        (Mod.name mod_ |> function
         | Some x -> x
         | _ -> assert false)
        fields
    in
    Mod.add_type mod_ new_type;
    Value.Atom "nil"

  and eval_def mod_ name value =
    let value = eval mod_ value in
    Env.update (Mod.get_env mod_) name ~f:(fun _ -> value);
    Value.Atom "nil"

  and eval_defmod mod_ name e =
    let new_module = Mod.create ~parent:(Some mod_) ~name:(Some name) e in
    Env.update (Mod.get_env mod_) ~f:(fun _ -> Module new_module) name;
    Env.update (Mod.get_env new_module) ~f:(fun _ -> Module new_module) name;
    Value.Module new_module

  and eval_mod mod_ e =
    let new_mod = Mod.create ~parent:(Some mod_) e in
    Value.Module new_mod

  and eval_atom_literal mod_ v =
    if String.(v = ":ls") then F.pr "%s" (Env.show Value.pp (Mod.get_env mod_));
    Value.Atom v

  and eval_mod_access mod_ module_name field =
    match eval mod_ module_name with
    | Module m -> (
        match Env.find (Mod.get_env m) field with
        | Some v -> v
        | None -> Value.Atom "nil")
    | Trait t -> (
        let to_mod = Trait.to_mod t in
        match Env.find (Mod.get_env to_mod) field with
        | Some v -> v
        | None -> Atom "nil")
    | _ -> failwith "not a module"

  and eval_fun mod_ args body =
    let args =
      List.map
        ~f:(function
          | Atom x -> x
          | _ -> assert false)
        args
    in
    handle_defun args body mod_

  and eval_map mod_ pairs =
    match define_map mod_ pairs with
    | Ok res -> res
    | Error key -> raise @@ KeyError key

  and eval_import mod_ imported =
    let new_module = Value.Module (Mod.with_file imported) in
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

  (*
  and eval_open mod_ imported =
    let new_mod = Mod.with_file imported in
    let new_env = Env.push_frame (Mod.get_env mod_) (Mod.get_env new_mod) in
    Env.pairs new_env
    |> List.iter ~f:(fun (key, value) ->
           Env.update (Mod.get_env mod_) key ~f:(fun _ -> value));
    Value.Module new_mod
*)

  and eval_named_call mod_ name args =
    match eval mod_ (Atom name) with
    | Function f -> handle_call mod_ (Function f) name args
    | other -> raise @@ Value.TypeError (other, unreachable ())

  and eval_call mod_ func args =
    match eval mod_ func with
    | Function f -> handle_call mod_ (Function f) "" args
    | other -> raise @@ Value.TypeError (other, unreachable ())

  and eval : Mod.t -> expr -> Value.t =
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
    | Int i -> Value.Int i
    | Float f -> Value.Float f
    | Quoted (List exprs) -> Value.List (List.map exprs ~f:(eval mod_))
    | Atom name -> Env.find_exn (Mod.get_env mod_) name
    | String s -> Value.String s
    | ConsCell (car, cdr) -> Value.ConsCell (eval mod_ car, eval mod_ cdr)
    | List [] -> Value.List []
    | Quoted e -> Value.Thunk (fun _ -> eval mod_ e)
    | Eof -> Atom "nil"
    | Set ys -> Value.Set (List.map ~f:(eval mod_) ys)

  and unreachable () = Value.Function (`Internal (fun _ -> failwith "unreachable"))

  and populate () =
    let items = Builtins.items () in
    let tbl =
      [
        Hashtbl.create ~growth_allowed:true
          ~size:(int_of_float (float_of_int (List.length items) *. 1.5))
          (module String);
      ]
    in
    Env.mass_add (Env.of_tbl_list tbl) items;
    Env.of_tbl_list tbl
end

and Parse : sig
  val parse : string -> expr
  val eval_put : Out_channel.t -> Mod.t -> string -> unit
  val evaluate_program : Mod.t -> string -> unit
  val evaluate_expr : Mod.t -> expr -> unit
  val get_text : string -> unit -> Mod.t
end = struct
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
        Printf.fprintf oc "%s\n" @@ Value.show @@ List.hd_exn evaluated
    | _ -> ()

  let evaluate_program (env : Mod.t) (s : string) = eval_put Out_channel.stdout env s

  let evaluate_expr env expr =
    match expr with
    | List exprs ->
        let evaluated = List.map exprs ~f:(Eval.eval env) |> List.rev in
        Printf.fprintf Out_channel.stdout "%s\n"
        @@ Value.show @@ List.hd_exn evaluated
    | _ -> ()

  let get_text filename () =
    let env = Mod.with_file filename in
    env
end

and Types : sig
  type t [@@deriving eq, show, ord]
  type value [@@deriving eq, show, ord]

  val create : string -> string list -> t
  val parent : t -> string
  val fields : t -> string list
  val field_exists : t -> string -> bool
  val get : value -> string -> Value.t option
  val get_type : value -> t
  val inner : value -> Value.t MlispMap.t
  val construct : t -> Value.t MlispMap.t -> value option
end = struct
  type t = { parent : string; field_names : string list } [@@deriving eq, show, ord]
  type value = t * Value.t MlispMap.t [@@deriving eq, show, ord]

  let create parent field_names = { parent; field_names }
  let parent { parent; field_names = _ } = parent
  let fields { parent = _; field_names } = field_names

  let field_exists { parent = _; field_names } field =
    List.mem field_names field ~equal:String.equal

  let construct { parent; field_names } map =
    let keys = List.map ~f:(fun (key, _value) -> key) @@ MlispMap.pairs map in
    if List.for_all keys ~f:(field_exists { parent; field_names }) then
      Some ({ parent; field_names }, map)
    else None

  let inner (_, contents) = contents
  let get_type (t, _) = t
  let get (_type, contents) field = MlispMap.get contents field
end

and Trait : sig
  type f [@@deriving show, eq, ord]
  type t [@@deriving eq, ord]

  val make_f : string -> int -> f
  val make_trait : string -> f list -> t
  val add_implementer : t -> string -> Mod.t -> unit
  val to_mod : t -> Mod.t
  val has_implementer : t -> string -> bool
  val impls : t -> (string * Mod.t) list
end = struct
  type f = { name : string; args : int } [@@deriving show, eq, ord]
  type t = { name : string; functions : f list; impls : (string, Mod.t) Hashtbl.t }

  let equal _ _ = false
  and compare _ _ = Int.max_value
  and make_f name args = { name; args }
  and has_implementer { impls; _ } name = Hashtbl.find impls name |> Option.is_some

  and impls { impls; _ } =
    let keys = Hashtbl.keys impls in
    let vals = List.filter_map keys ~f:(Hashtbl.find impls) in
    List.zip_exn keys vals

  and make_trait name functions =
    {
      name;
      functions;
      impls = (Hashtbl.create ~growth_allowed:true ~size:10) (module String);
    }

  let add_implementer x name i = Hashtbl.update x.impls name ~f:(fun _ -> i)

  let to_mod { functions; impls; name } =
    let get_impl (func : f) args =
      Hashtbl.keys impls |> List.iter ~f:print_endline;
      match args with
      | [] -> assert false
      | Value.UserDefined t :: _ -> (
          let value_type = Types.get_type t in
          match Hashtbl.find impls (Types.parent value_type) with
          | Some m -> (
              match Env.find (Mod.get_env m) func.name with
              | Some (Function (`Userdefined (func, env))) ->
                  let f =
                    Eval.handle_userdef_call
                      (Mod.remake (Env.of_tbl_list env) None)
                      func
                    |> Eval.funcall
                  in
                  f args
              | _ ->
                  failwithf "trait %s is not implemented for %s" name
                    (Types.parent value_type) ())
          | _ -> failwithf "module %s found" (Types.parent value_type) ())
      | _ ->
          failwith
            "first argument of trait function must be a module associated type"
    in
    let new_functions =
      functions
      |> List.map ~f:(fun { name; args } -> (name, get_impl { name; args }))
    in
    let new_mod = Mod.make_new (Some name) in
    List.iter new_functions ~f:(fun (name, fn) ->
        Env.update (Mod.get_env new_mod) name ~f:(fun _ -> Function (`Internal fn)));
    new_mod
end
