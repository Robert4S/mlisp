open Common_types
open Core

module Create (Eval : EVAL) = struct
  let rec items _ =
    [
      ("true", Atom "true");
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
    | Int a, Int b -> Int (a + b)
    | Float a, Float b -> Float (a +. b)
    | Int a, Float b -> Float (b +. float_of_int a)
    | Float a, Int b -> Float (a +. float_of_int b)
    | String a, String b -> String (String.append a b)
    | Set a, Set b -> Set (List.dedup_and_sort ~compare:Value.compare (a @ b))
    | _ -> raise (TypeError (a, b))

  and bin_minus a b =
    match (a, b) with
    | Int a, Int b -> Int (a - b)
    | Float a, Float b -> Float (a -. b)
    | Int a, Float b -> Float (float_of_int a -. b)
    | Float a, Int b -> Float (a -. float_of_int b)
    | _ ->
        (*         print_endline @@ Value.show a; *)
        raise (TypeError (a, b))

  and bin_mul a b =
    match (a, b) with
    | Int a, Int b -> Int (a * b)
    | Float a, Float b -> Float (a *. b)
    | Int a, Float b -> Float (float_of_int a *. b)
    | Float a, Int b -> Float (a *. float_of_int b)
    | _ -> raise (TypeError (a, b))

  and bin_div a b =
    match (a, b) with
    | Int a, Int b -> Int (a / b)
    | Float a, Float b -> Float (a /. b)
    | Int a, Float b -> Float (float_of_int a /. b)
    | Float a, Int b -> Float (a /. float_of_int b)
    | _ -> raise (TypeError (a, b))

  and bin_gt a b =
    match (a, b) with
    | Int a, Int b -> bool_to_atom (a > b)
    | Float a, Float b -> bool_to_atom Float.(a > b)
    | Int a, Float b -> bool_to_atom Float.(float_of_int a > b)
    | Float a, Int b -> bool_to_atom Float.(a > float_of_int b)
    | _ -> raise (TypeError (a, b))

  and bin_lt a b =
    match (a, b) with
    | Int a, Int b -> bool_to_atom (a < b)
    | Float a, Float b -> bool_to_atom Float.(a < b)
    | Int a, Float b -> bool_to_atom Float.(float_of_int a < b)
    | Float a, Int b -> bool_to_atom Float.(a < float_of_int b)
    | _ -> raise (TypeError (a, b))

  and bin_gte a b =
    match bin_gt a b with
    | Atom "true" -> Atom "true"
    | Atom "false" -> bool_to_atom @@ Value.equal a b
    | other -> failwith (Value.show other)

  and bin_lte a b =
    match bin_lt a b with
    | Atom "true" -> Atom "true"
    | Atom "false" -> bool_to_atom @@ Value.equal a b
    | other -> failwith (Value.show other)

  and bin_mod a b =
    match (a, b) with
    | Int a, Int b -> Int (a mod b)
    | _ -> raise (TypeError (a, b))

  and str vals =
    String
      (List.fold vals ~init:"" ~f:(fun acc -> function
         | String s -> String.append acc s
         | value -> String.append acc @@ Value.show value))

  and io_puts value =
    print_endline @@ Value.show @@ List.hd_exn value;
    Atom "nil"

  and eq (vals : Value.t list) : Value.t =
    bool_to_atom @@ Option.is_some @@ List.all_equal ~equal:Value.equal vals

  and add (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_add in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal add), vals))

  and sub (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_minus in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal sub), vals))

  and mul (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_mul in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal mul), vals))

  and div (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_div in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal div), vals))

  and gt (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_gt in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal gt), vals))

  and lt (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_lt in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal lt), vals))

  and gte (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_gte in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal gte), vals))

  and lte (vals : Value.t list) : Value.t =
    let reduced = List.reduce vals ~f:bin_lte in
    match reduced with
    | Some o -> o
    | None -> raise (ArgError (Function (`Internal lte), vals))

  and if_ vals =
    match vals with
    | [ Thunk pred; Thunk succ; Thunk els ] -> if_ [ pred (); Thunk succ; Thunk els ]
    | [ Atom "true"; Thunk succ; _ ] -> succ ()
    | [ Atom "false"; _; Thunk els ] -> els ()
    | _ -> raise (ArgError (Function (`Internal if_), vals))

  and mod_ vals =
    match vals with
    | [ a; b ] -> bin_mod a b
    | _ -> raise (ArgError (Function (`Internal mod_), vals))

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
        raise (MatchError "No cond clause matched. Perhaps a missing else clause?")
    | _ -> failwith "cond body should be a thunk"

  and nil (vals : Value.t list) : Value.t =
    match vals with
    | [ List [] ] -> Atom "true"
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
    | [ x; List ls ] -> List (x :: ls)
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
    | [ MMap m ] -> List (List.map ~f:(fun (k, _) -> Atom k) @@ MMap.pairs m)
    | _ -> assert false

  and values : Value.t list -> Value.t = function
    | [ MMap m ] -> List (List.map ~f:(fun (_, v) -> v) @@ MMap.pairs m)
    | _ -> assert false

  and pairs : Value.t list -> Value.t = function
    | [ MMap m ] ->
        List (List.map ~f:(fun (k, v) -> ConsCell (Atom k, v)) @@ MMap.pairs m)
    | _ -> assert false

  and get_map : Value.t list -> Value.t = function
    | [ x; MMap m ] -> (
        let res = MMap.get m @@ Value.show x in
        match res with
        | Some v -> v
        | None -> Atom "nil")
    | _ -> assert false

  and ref_ : Value.t list -> Value.t = function
    | [ x ] -> Ref (ref x)
    | _ -> assert false

  and set : Value.t list -> Value.t = function
    | [ Ref y; v ] ->
        let prev = !y in
        y := v;
        prev
    | _ -> assert false

  and get : Value.t list -> Value.t = function
    | [ Ref y ] -> !y
    | _ -> assert false

  and bool_to_atom a = if a then Atom "true" else Atom "false"

  and hd_ (vals : Value.t list) =
    match vals with
    | [ List (x :: _) ] -> x
    | [ List [] ] -> Atom "nil"
    | [ ConsCell (car, _cdr) ] -> car
    | [ String s ] -> String (Char.to_string @@ String.get s 0)
    | [ Function (`Userdefined (func, env)) ] -> hd_ @@ [ eval_function func env ]
    | _ ->
        raise
          (InvalidArg "cannot take the head of something that is not a collection")

  and tail vals =
    match vals with
    | [ List (_ :: xs) ] -> List xs
    | [ List [] ] -> Atom "nil"
    | [ ConsCell (_car, cdr) ] -> cdr
    | [ Function (`Userdefined (func, env)) ] -> tail @@ [ eval_function func env ]
    | _ -> raise (InvalidArg "tail can only take the tail of a single list")

  and quit _vals =
    print_endline "Goodbye!";
    exit 0

  and eval_function func (env : mod_t) = Eval.handle_userdef_call env func []

  and get_comb = function
    | [ v; MMap m ] -> get_map [ v; MMap m ]
    | [ Ref x ] -> get [ Ref x ]
    | _ -> assert false
end
