open Common_types
open Core

module Builtins (Mod : sig
  type t [@@deriving eq, ord]
end) (Value : sig
  type userdef_t
  type trait_t

  type t =
    | Int of int
    | Float of float
    | Atom of string
    | Function of
        [ `Userdefined of func * t gen_hashtable | `Internal of t gen_func ]
    | String of string
    | List of t list
    | Thunk of t delayed
    | ConsCell of t * t
    | Map of t MMap.t
    | Ref of t ref
    | Set of t list
    | Module of Mod.t
    | UserDefined of userdef_t
    | Trait of trait_t
  [@@deriving eq, ord, show, sexp]

  exception TypeError of t * t
  exception ArgError of t * t list

  val typeof : t list -> t
end) (Eval : sig
  val handle_userdef_call : Mod.t -> func -> Value.t gen_func
  val remake : ?name:string option -> Value.t Env.t -> Mod.t
end) : sig
  val items : unit -> (string * Value.t) list
end = struct
  let rec items _ =
    [
      ("true", Value.Atom "true");
      ("else", Atom "true");
      ("false", Atom "false");
      ("nil", Atom "nil");
      ("+", Function (`Internal add));
      ("-", Function (`Internal sub));
      ("*", Function (`Internal mul));
      ("/", Function (`Internal div));
      (">", Function (`Internal gt));
      ("<", Function (`Internal lt));
      (">=", Function (`Internal gte));
      ("<=", Function (`Internal lte));
      ("=", Function (`Internal eq));
      ("mod", Function (`Internal mod_));
      ("if", Function (`Internal if_));
      ("str", Function (`Internal str));
      ("io-puts", Function (`Internal io_puts));
      ("cond", Function (`Internal cond));
      ("tail", Function (`Internal tail));
      ("cons", Function (`Internal cons));
      ("car", Function (`Internal car));
      ("cdr", Function (`Internal cdr));
      ("hd", Function (`Internal hd_));
      ("keys", Function (`Internal keys));
      ("values", Function (`Internal values));
      ("pairs", Function (`Internal pairs));
      ("quit", Function (`Internal quit));
      ("type", Function (`Internal Value.typeof));
      ("get", Function (`Internal get_comb));
      ("ref", Function (`Internal ref_));
      ("set", Function (`Internal set));
      ("nil?", Function (`Internal nil));
      ("list?", Function (`Internal list));
      ("fun?", Function (`Internal fun_));
      ("cell?", Function (`Internal cell));
    ]

  and bin_add a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> Value.Int (a + b)
    | Float a, Float b -> Float (a +. b)
    | Int a, Float b -> Float (b +. float_of_int a)
    | Float a, Int b -> Float (a +. float_of_int b)
    | String a, String b -> String (String.append a b)
    | Set a, Set b -> Set (List.dedup_and_sort ~compare:Value.compare (a @ b))
    | _ -> raise (Value.TypeError (a, b))

  and bin_minus a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> Value.Int (a - b)
    | Float a, Float b -> Float (a -. b)
    | Int a, Float b -> Float (float_of_int a -. b)
    | Float a, Int b -> Float (a -. float_of_int b)
    | _ ->
        print_endline @@ Value.show a;
        raise (Value.TypeError (a, b))

  and bin_mul a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> Value.Int (a * b)
    | Float a, Float b -> Float (a *. b)
    | Int a, Float b -> Float (float_of_int a *. b)
    | Float a, Int b -> Float (a *. float_of_int b)
    | _ -> raise (Value.TypeError (a, b))

  and bin_div a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> Value.Int (a / b)
    | Float a, Float b -> Float (a /. b)
    | Int a, Float b -> Float (float_of_int a /. b)
    | Float a, Int b -> Float (a /. float_of_int b)
    | _ -> raise (Value.TypeError (a, b))

  and bin_gt a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> bool_to_atom (a > b)
    | Float a, Float b -> bool_to_atom Float.(a > b)
    | Int a, Float b -> bool_to_atom Float.(float_of_int a > b)
    | Float a, Int b -> bool_to_atom Float.(a > float_of_int b)
    | _ -> raise (Value.TypeError (a, b))

  and bin_lt a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> bool_to_atom (a < b)
    | Float a, Float b -> bool_to_atom Float.(a < b)
    | Int a, Float b -> bool_to_atom Float.(float_of_int a < b)
    | Float a, Int b -> bool_to_atom Float.(a < float_of_int b)
    | _ -> raise (Value.TypeError (a, b))

  and bin_gte a b =
    match bin_gt a b with
    | Value.Atom "true" -> Value.Atom "true"
    | Atom "false" -> bool_to_atom @@ Value.equal a b
    | other -> failwith (Value.show other)

  and bin_lte a b =
    match bin_lt a b with
    | Value.Atom "true" -> Value.Atom "true"
    | Atom "false" -> bool_to_atom @@ Value.equal a b
    | other -> failwith (Value.show other)

  and bin_mod a b =
    match (a, b) with
    | Value.Int a, Value.Int b -> Value.Int (a mod b)
    | _ -> raise (Value.TypeError (a, b))

  and str vals =
    Value.String
      (List.fold vals ~init:"" ~f:(fun acc value ->
           String.append acc @@ Value.show value))

  and io_puts value =
    print_endline @@ Value.show @@ List.hd_exn value;
    Value.Atom "nil"

  and eq (vals : Value.t list) : Value.t =
    bool_to_atom @@ Option.is_some @@ List.all_equal ~equal:Value.equal vals

  and add (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_add in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal add), vals))

  and sub (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_minus in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal sub), vals))

  and mul (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_mul in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal mul), vals))

  and div (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_div in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal div), vals))

  and gt (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_gt in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal gt), vals))

  and lt (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_lt in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal lt), vals))

  and gte (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_gte in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal gte), vals))

  and lte (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_lte in
    match reduced with
    | Some o -> o
    | None -> raise (Value.ArgError (Function (`Internal lte), vals))

  and if_ vals =
    match vals with
    | [ Value.Thunk pred; Thunk succ; Thunk els ] ->
        if_ [ pred (); Thunk succ; Thunk els ]
    | [ Atom "true"; Thunk succ; _ ] -> succ ()
    | [ Atom "false"; _; Thunk els ] -> els ()
    | _ -> raise (Value.ArgError (Function (`Internal if_), vals))

  and mod_ vals =
    match vals with
    | [ a; b ] -> bin_mod a b
    | _ -> raise (Value.ArgError (Function (`Internal mod_), vals))

  and zip_pairs lst =
    let rec aux lst acc =
      match lst with
      | [] -> List.rev acc
      | a :: b :: xs -> aux xs ((a, b) :: acc)
      | _ -> failwith "invalid"
    in
    aux lst []

  and cond (vals : Value.t list) : Value.t =
    if not (List.length vals % 2 = 0) then
      raise (InvalidArg "argument count must be even");
    let pred_thens = zip_pairs vals in
    let res =
      List.find pred_thens ~f:(fun (pred, _) ->
          match pred with
          | Atom "true" -> true
          | Thunk pred -> Value.equal (pred ()) (Atom "true")
          | _ -> false)
    in
    match res with
    | Some (_, Thunk v) -> v ()
    | None ->
        raise
          (MatchError "No cond clause matched. Perhaps a missing else clause?")
    | _ -> failwith "cond body should be a thunk"

  and nil (vals : Value.t list) : Value.t =
    match vals with
    | [ Value.List [] ] -> Value.Atom "true"
    | [ Atom "nil" ] -> Atom "true"
    | [ _ ] -> Atom "false"
    | _ -> raise (InvalidArg "nil? expects 1 argument")

  and list (vals : Value.t list) : Value.t =
    match vals with
    | [ List _ ] -> Atom "true"
    | [ _ ] -> Atom "false"
    | _ -> raise (InvalidArg "list? expects 1 argument")

  and cell (vals : Value.t list) : Value.t =
    match vals with
    | [ ConsCell _ ] -> Atom "true"
    | [ _ ] -> Atom "false"
    | _ -> raise (InvalidArg "list? expects 1 argument")

  and cons (vals : Value.t list) : Value.t =
    match vals with
    | [ x; Value.List ls ] -> Value.List (x :: ls)
    | [ x; Atom "nil" ] -> List [ x ]
    | [ x; y ] -> ConsCell (x, y)
    | _ -> raise (InvalidArg "cons expects a value and a list")

  and car (vals : Value.t list) : Value.t =
    match vals with
    | [ ConsCell (car, _cdr) ] -> car
    | [ List (x :: _) ] -> x
    | _ -> raise (InvalidArg "cannot get the car off of a non cons list")

  and cdr (vals : Value.t list) : Value.t =
    match vals with
    | [ ConsCell (_car, cdr) ] -> cdr
    | [ List (_ :: xs) ] -> List xs
    | _ -> raise (InvalidArg "cannot get the cdr off of a non cons list")

  and fun_ (vals : Value.t list) : Value.t =
    match vals with
    | [ Function _ ] -> Atom "true"
    | [ _ ] -> Atom "false"
    | _ -> raise (InvalidArg "fun? expects one argument")

  and keys : Value.t list -> Value.t = function
    | [ Value.Map m ] ->
        Value.List (List.map ~f:(fun (k, _) -> Value.Atom k) @@ MMap.pairs m)
    | _ -> assert false

  and values : Value.t list -> Value.t = function
    | [ Value.Map m ] ->
        Value.List (List.map ~f:(fun (_, v) -> v) @@ MMap.pairs m)
    | _ -> assert false

  and pairs : Value.t list -> Value.t = function
    | [ Value.Map m ] ->
        Value.List
          (List.map ~f:(fun (k, v) -> Value.ConsCell (Value.Atom k, v))
          @@ MMap.pairs m)
    | _ -> assert false

  and get_map : Value.t list -> Value.t = function
    | [ x; Value.Map m ] -> (
        let res = MMap.get m @@ Value.show x in
        match res with Some v -> v | None -> Value.Atom "nil")
    | _ -> assert false

  and ref_ : Value.t list -> Value.t = function
    | [ x ] -> Value.Ref (ref x)
    | _ -> assert false

  and set : Value.t list -> Value.t = function
    | [ Value.Ref y; v ] ->
        let prev = !y in
        y := v;
        prev
    | _ -> assert false

  and get : Value.t list -> Value.t = function
    | [ Value.Ref y ] -> !y
    | _ -> assert false

  and bool_to_atom a = if a then Value.Atom "true" else Atom "false"

  and hd_ (vals : Value.t list) =
    match vals with
    | [ Value.List (x :: _) ] -> x
    | [ List [] ] -> Atom "nil"
    | [ ConsCell (car, _cdr) ] -> car
    | [ String s ] -> String (Char.to_string @@ String.get s 0)
    | [ Function (`Userdefined (func, env)) ] ->
        let env = Env.of_tbl_list env in
        hd_ @@ [ eval_function func env ]
    | _ ->
        raise
          (InvalidArg
             "cannot take the head of something that is not a collection")

  and tail vals =
    match vals with
    | [ Value.List (_ :: xs) ] -> Value.List xs
    | [ List [] ] -> Atom "nil"
    | [ ConsCell (_car, cdr) ] -> cdr
    | [ Function (`Userdefined (func, env)) ] ->
        tail @@ [ eval_function func (Env.of_tbl_list env) ]
    | _ -> raise (InvalidArg "tail can only take the tail of a single list")

  and quit _vals =
    print_endline "Goodbye!";
    exit 0

  and eval_function func (env : Value.t Env.t) =
    Eval.handle_userdef_call (Eval.remake env) func []

  and get_comb = function
    | [ v; Value.Map m ] -> get_map [ v; Map m ]
    | [ Ref x ] -> get [ Ref x ]
    | _ -> assert false
end
