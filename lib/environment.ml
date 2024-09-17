open Core
open Ast
open Common_types
module F = Fmt

module rec Builtins : sig
  val items : unit -> (string * Value.t) list
end =
  Prelude.Builtins (Mod) (MlispMap) (Value) (Env) (Eval)

and MlispMap : sig
  type 'a t [@@deriving eq, ord]

  val create : (string * 'a) list -> ('a t, string) result
  val get : 'a t -> string -> 'a option
  val pairs : 'a t -> (string * 'a) list
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = 'a String.Map.t [@@deriving eq, ord]

  let create pairs : ('a t, string) result =
    match String.Map.of_alist pairs with
    | `Ok m -> Ok m
    | `Duplicate_key s -> Error s

  let get = Map.find
  let pairs m = Map.to_alist m

  let pp pp_value formatter map =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "%s = %a" key pp_value value
    in
    Format.fprintf formatter "{";
    let pairs = Map.to_alist map in
    (match pairs with
    | [] -> ()
    | [ pair ] -> pp_pair formatter pair
    | pair :: pairs ->
        pp_pair formatter pair;
        List.iter pairs ~f:(fun pair ->
            Format.fprintf formatter ", ";
            pp_pair formatter pair));
    Format.fprintf formatter "}"
end

and Value : sig
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
  [@@deriving eq, ord, show, sexp]

  exception TypeError of t * t
  exception ArgError of t * t list
end = struct
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

  let show value = Format.asprintf "%a" pp value
  let sexp_of_t = sexp_of_opaque
  let t_of_sexp = opaque_of_sexp
end

and Mod : (sig
  type env_t
  type t [@@deriving eq, ord]

  val with_file : string -> t
  val create : expr -> t
  val get_env : t -> env_t
end
with type env_t = Env.t) = struct
  type t = { env : Env.t }
  type env_t = Env.t

  let with_file name =
    let contents = In_channel.read_all (name ^ ".mlisp") in
    let env = Env.populate () in
    Parse.evaluate_program env contents;
    { env }

  let create expr =
    let env = Env.populate () in
    Parse.evaluate_expr env expr;
    { env }

  let get_env { env } = env
  let equal _ _ = false
  let compare _ _ = Int.max_value
end

and Eval : sig
  val eval : Env.t -> expr -> Value.t
  val handle_userdef_call : Value.t gen_hashtable -> func -> Value.t gen_func
  val bound_frame : string list -> expr -> Env.t -> Env.t
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
    | Fn (_, e)
    | Defun (_, _, e)
    | Def (_, e)
    | Quoted e ->
        vars e
    | _ -> []

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

  and handle_defun args body (env : Env.t) =
    let captured = Env.to_tbl_list @@ bound_frame args body env in
    Value.Function (`Userdefined ({ args; body }, captured))

  and handle_userdef_call
      (env : Value.t gen_hashtable)
      (f : func)
      (args : Value.t list) =
    let env = Env.of_tbl_list env in
    let env = Env.push env in
    if not (List.length args = List.length f.args) then
      raise (Value.ArgError (Function (`Userdefined (f, Env.to_tbl_list env)), args));
    List.iter (List.zip_exn f.args args) ~f:(fun (name, value) ->
        Env.update env name ~f:(fun _ -> value));
    eval env f.body

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

  and handle_call env func name args =
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
        funcall (handle_userdef_call new_env f) args
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

  and handle_map_access env m field =
    match eval env m with
    | Value.Map m -> (
        match MlispMap.get m field with
        | Some v -> v
        | None -> Atom "nil")
    | _ -> raise @@ SyntaxError "temporary problem"

  and define_map env args =
    let pairs =
      List.chunks_of args ~length:2
      |> List.map ~f:(function
           (* make map keys be any value, not just atoms *)
           | [ a; b ] -> (Value.show @@ eval env a, b)
           | _ -> assert false)
      |> List.map ~f:(fun (key, value) -> (key, eval env value))
    in
    Result.(MlispMap.create pairs >>= fun pairs -> Ok (Value.Map pairs))

  and eval (env : Env.t) (value : expr) : Value.t =
    match value with
    | DefMod e ->
        let new_mod = Mod.create e in
        Module new_mod
    | Atom v when Char.equal ':' (String.to_array v).(0) ->
        if String.(v = ":ls") then F.pr "%s" (Env.show env);
        Value.Atom v
    | ModAccess (module_name, field) -> (
        match eval env module_name with
        | Module m -> (
            match Env.find (Mod.get_env m) field with
            | Some v -> v
            | None -> Atom "nil")
        | _ -> failwith "not a module")
    | Fn (args, body) -> handle_defun args body env
    | Defun (name, args, body) ->
        let func = handle_defun args body env in
        Env.update env name ~f:(fun _ -> func);
        (match func with
        | Function (`Userdefined (_, captured)) ->
            Env.update (Env.of_tbl_list captured) name ~f:(fun _ -> func)
        | _ -> ());
        Value.Atom "nil"
    | Map pairs -> (
        match define_map env pairs with
        | Ok res -> res
        | Error key -> raise @@ KeyError key)
    | List [ Atom "import"; Atom imported ] ->
        let new_module = Value.Module (Mod.with_file imported) in
        Env.update env imported ~f:(fun _ -> new_module);
        new_module
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
        handle_userdef_call (Env.to_tbl_list env) { args = names; body } values
    | List [ Atom "open"; Atom imported ] ->
        let new_mod = Mod.with_file imported in
        let new_env = Env.push_frame env (Mod.get_env new_mod) in
        Env.pairs new_env
        |> List.iter ~f:(fun (key, value) -> Env.update env key ~f:(fun _ -> value));
        Module new_mod
    | List [ Atom "get"; field; m ] ->
        handle_map_access env m (Value.show @@ eval env field)
    | List [ Atom n; e ] when Char.equal (String.to_array n).(0) ':' ->
        handle_map_access env e n
    | List (Atom name :: args) -> (
        match eval env (Atom name) with
        | Function f -> handle_call env (Function f) name args
        | other -> raise @@ Value.TypeError (other, Env.unreachable ()))
    | List (func :: args) -> (
        match eval env func with
        | Function f -> handle_call env (Function f) "" args
        | other -> raise @@ Value.TypeError (other, Env.unreachable ()))
    | Def (name, value) ->
        let value = eval env value in
        Env.update env name ~f:(fun _ -> value);
        Value.Atom "nil"
    | Int i -> Value.Int i
    | Float f -> Value.Float f
    | Quoted (List exprs) -> Value.List (List.map exprs ~f:(eval env))
    | Atom name -> Env.find_exn env name
    | String s -> Value.String s
    | ConsCell (car, cdr) -> Value.ConsCell (eval env car, eval env cdr)
    | List [] -> Value.List []
    | Quoted e -> Value.Thunk (fun _ -> eval env e)
    | Eof -> Atom "nil"
    | Set ys -> Value.Set (List.map ~f:(eval env) ys)
end

and Env : sig
  type t

  val of_tbl_list : Value.t gen_hashtable -> t
  val to_tbl_list : t -> Value.t gen_hashtable
  val populate : unit -> t
  val find : t -> string -> Value.t option
  val make : (string * Value.t) list -> t
  val push : t -> t
  val update : t -> string -> f:(Value.t option -> Value.t) -> unit
  val find_exn : t -> string -> Value.t
  val unreachable : unit -> Value.t
  val show : t -> string
  val push_frame : t -> t -> t
  val pairs : t -> (string * Value.t) list
end = struct
  type t = Value.t gen_hashtable

  let rec pairs x =
    let x = to_tbl_list x in
    let keys = List.bind x ~f:Hashtbl.keys in
    let values = List.filter_map keys ~f:(find x) in
    List.zip_exn keys values

  and unreachable () = Value.Function (`Internal (fun _ -> failwith "unreachable"))
  and of_tbl_list (x : Value.t gen_hashtable) : t = x
  and to_tbl_list (x : t) : Value.t gen_hashtable = x
  and push_frame env frames = frames @ env

  and populate () =
    let items = Builtins.items () in
    let tbl =
      [
        Hashtbl.create ~growth_allowed:true
          ~size:(int_of_float (float_of_int (List.length items) *. 1.5))
          (module String);
      ]
    in
    mass_add tbl items;
    tbl

  and update e k ~f =
    let head = List.hd_exn e in
    Hashtbl.update head k ~f

  and push e =
    let symbol_table =
      Hashtbl.create ~growth_allowed:true ~size:20 (module String)
    in
    symbol_table :: e

  and show (e : t) =
    let open List.Let_syntax in
    let keys = e >>= Hashtbl.keys in
    let data = e >>= Hashtbl.data >>| Value.show in
    String.concat ~sep:"\n"
    @@ List.map ~f:(fun (a, b) -> sprintf "(%s : %s)" a b)
    @@ List.sort ~compare:(fun (key1, _) (key2, _) -> String.compare key1 key2)
    @@ List.zip_exn keys data

  and find e name =
    match e with
    | [] -> None
    | x :: xs -> (
        let found = Hashtbl.find x name in
        match found with
        | Some item -> Some item
        | None -> find xs name)

  and find_exn e name =
    match find e name with
    | None -> raise (Unbound name)
    | Some v -> v

  and mass_add env lst =
    let head = List.hd_exn env in
    match lst with
    | [] -> ()
    | (key, data) :: xs ->
        Hashtbl.add_exn head ~key ~data;
        mass_add env xs

  and make pairs =
    let tbl =
      [
        Hashtbl.create ~growth_allowed:true
          ~size:(int_of_float (float_of_int (List.length pairs) *. 1.5))
          (module String);
      ]
    in
    mass_add tbl pairs;
    tbl
end

and Parse : sig
  val parse : string -> expr
  val eval_put : Out_channel.t -> Env.t -> string -> unit
  val evaluate_program : Env.t -> string -> unit
  val evaluate_expr : Env.t -> expr -> unit
  val get_text : Out_channel.t -> string -> unit -> Env.t
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

  let evaluate_program (env : Env.t) (s : string) = eval_put Out_channel.stdout env s

  let evaluate_expr env expr =
    match expr with
    | List exprs ->
        let evaluated = List.map exprs ~f:(Eval.eval env) |> List.rev in
        Printf.fprintf Out_channel.stdout "%s\n"
        @@ Value.show @@ List.hd_exn evaluated
    | _ -> ()

  let get_text oc filename () =
    let text = In_channel.read_all filename in
    let env = Env.populate () in
    eval_put oc env text;
    env
end
