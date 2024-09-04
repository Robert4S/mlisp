open Core
open Ast
module F = Fmt

exception InvalidArg of string
exception MatchError of string
exception Unbound of string

type 'a gen_func = 'a list -> 'a
type 'a delayed = unit -> 'a
type 'a gen_hashtable = (string, 'a) Hashtbl.t list
type 'a cons_cell = 'a * 'a [@@deriving show, eq, ord]
type func = { args : string list; body : expr } [@@deriving show, eq, ord]

let compare_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = 0
and equal_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = false
and pp_gen_func _c f (_gf : 'a gen_func) = Format.fprintf f "function"
and compare_delayed _c _a _b = 0
and equal_delayed _c _a_ _b = false
and pp_delayed _c f _gf = Format.fprintf f "function"
and compare_gen_hashtable _c _a _b = 0
and equal_gen_hashtable _c _a _b = false
and pp_gen_hashtable _c f _gf = Format.fprintf f "TABLE"

module MlispMap : sig
  type 'a t [@@deriving eq, ord]

  val create : (string * 'a) list -> ('a t, string) result
  val get : 'a t -> string -> 'a option
  val pairs : 'a t -> (string * 'a) list
end = struct
  type 'a t = 'a String.Map.t [@@deriving eq, ord]

  let create pairs : ('a t, string) result =
    match String.Map.of_alist pairs with
    | `Ok m -> Ok m
    | `Duplicate_key s -> Error s

  let get = Map.find
  let pairs m = Map.to_alist m
end

type value =
  | Int of int
  | Float of float
  | Atom of string
  | Function of
      [ `Userdefined of func * value gen_hashtable | `Internal of value gen_func ]
  | String of string
  | List of value list
  | Thunk of value delayed
  | ConsCell of value cons_cell
  | Map of value MlispMap.t
[@@deriving eq, ord]

exception TypeError of value * value
exception ArgError of value * value list

type env = (string, value) Hashtbl.t list

let rec list_to_cons_cell (ls : value list) =
  let rec aux lst acc =
    match (lst, acc) with
    | [], ConsCell (a, b) -> ConsCell (ConsCell (a, b), Atom "nil")
    | [], other -> ConsCell (other, Atom "nil")
    | x :: xs, ConsCell (a, b) -> aux xs (ConsCell (x, ConsCell (a, b)))
    | x :: xs, other -> aux xs (ConsCell (x, other))
  in
  match ls with
  | x :: xs -> aux xs x
  | [] -> Atom "nil"

and is_valid_list (value : value) =
  match value with
  | Atom "nil" -> true
  | ConsCell (_, rest) -> is_valid_list rest
  | _ -> false

and pp_value (formatter : Format.formatter) value =
  match value with
  | Int i -> Format.fprintf formatter "%d" i
  | Float f -> Format.fprintf formatter "%f" f
  | Atom a -> Format.fprintf formatter "%s" a
  | Function _ -> Format.fprintf formatter "<func>"
  | String s -> Format.fprintf formatter "\"%s\"" s
  | List vals ->
      List.iter vals ~f:(fun value -> Format.fprintf formatter "%a " pp_value value)
  | Thunk _ -> Format.fprintf formatter "<thunk>"
  | ConsCell (car, cdr) ->
      Format.fprintf formatter "%a . %a" pp_value car pp_value cdr
  | Map m ->
      Format.fprintf formatter "{";
      let f (key, value) = Format.fprintf formatter ", %s = %a" key pp_value value in
      (match MlispMap.pairs m with
      | (key, value) :: xs ->
          Format.fprintf formatter "%s = %a" key pp_value value;
          List.iter xs ~f
      | [] -> ());
      Format.fprintf formatter "}"

and show_value value =
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  pp_value formatter value;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

and fake_func = Function (`Internal (fun _ -> failwith "not a real function"))
and bool_to_atom a = if a then Atom "true" else Atom "false"

and update e k ~f =
  let head = List.hd_exn e in
  Hashtbl.update head k ~f

and push e =
  let symbol_table = Hashtbl.create ~growth_allowed:true ~size:20 (module String) in
  symbol_table :: e

and show (e : env) =
  let open List.Let_syntax in
  let keys = e >>= Hashtbl.keys in
  let data = e >>= Hashtbl.data >>| show_value in
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

and bin_add a b =
  match (a, b) with
  | Int a, Int b -> Int (a + b)
  | Float a, Float b -> Float (a +. b)
  | Int a, Float b -> Float (b +. float_of_int a)
  | Float a, Int b -> Float (a +. float_of_int b)
  | String a, String b -> String (String.append a b)
  | _ -> raise (TypeError (a, b))

and bin_minus a b =
  match (a, b) with
  | Int a, Int b -> Int (a - b)
  | Float a, Float b -> Float (a -. b)
  | Int a, Float b -> Float (float_of_int a -. b)
  | Float a, Int b -> Float (a -. float_of_int b)
  | _ ->
      print_endline @@ show_value a;
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

and bin_eq = equal_value

and bin_gte a b =
  match bin_gt a b with
  | Atom "true" -> Atom "true"
  | Atom "false" -> bool_to_atom @@ equal_value a b
  | other -> failwith (show_value other)

and bin_lte a b =
  match bin_lt a b with
  | Atom "true" -> Atom "true"
  | Atom "false" -> bool_to_atom @@ equal_value a b
  | other -> failwith (show_value other)

and bin_mod a b =
  match (a, b) with
  | Int a, Int b -> Int (a mod b)
  | _ -> raise (TypeError (a, b))

and str vals =
  String
    (List.fold vals ~init:"" ~f:(fun acc value ->
         String.append acc @@ show_value value))

and io_puts value =
  print_endline @@ show_value @@ List.hd_exn value;
  Atom "nil"

and eq (vals : value list) : value =
  bool_to_atom @@ Option.is_some @@ List.all_equal ~equal:equal_value vals

and add (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_add in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal add), vals))

and sub (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_minus in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal sub), vals))

and mul (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_mul in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal mul), vals))

and div (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_div in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal div), vals))

and gt (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_gt in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal gt), vals))

and lt (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_lt in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal lt), vals))

and gte (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_gte in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal gte), vals))

and lte (vals : value list) : value =
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

and cond (vals : value list) : value =
  if not (List.length vals % 2 = 0) then
    raise (InvalidArg "argument count must be even");
  let pred_thens = zip_pairs vals in
  let res =
    List.find pred_thens ~f:(fun (pred, _) ->
        match pred with
        | Atom "true" -> true
        | Thunk pred -> equal_value (pred ()) (Atom "true")
        | _ -> false)
  in
  match res with
  | Some (_, Thunk v) -> v ()
  | None ->
      raise (MatchError "No cond clause matched. Perhaps a missing else clause?")
  | _ -> failwith "cond body should be a thunk"

and print_arg_list vals =
  List.iter vals ~f:(fun arg -> print_endline @@ show_value arg)

and nil (vals : value list) =
  match vals with
  | [ List [] ] -> Atom "true"
  | [ Atom "nil" ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "nil? expects 1 argument")

and list (vals : value list) =
  match vals with
  | [ List _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "list? expects 1 argument")

and cell (vals : value list) =
  match vals with
  | [ ConsCell _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "list? expects 1 argument")

and cons (vals : value list) =
  match vals with
  | [ x; List ls ] -> List (x :: ls)
  | [ x; Atom "nil" ] -> List [ x ]
  | [ x; y ] -> ConsCell (x, y)
  | _ -> raise (InvalidArg "cons expects a value and a list")

and car (vals : value list) =
  match vals with
  | [ ConsCell (car, _cdr) ] -> car
  | [ List (x :: _) ] -> x
  | _ -> raise (InvalidArg "cannot get the car off of a non cons list")

and cdr (vals : value list) =
  match vals with
  | [ ConsCell (_car, cdr) ] -> cdr
  | [ List (_ :: xs) ] -> List xs
  | _ -> raise (InvalidArg "cannot get the cdr off of a non cons list")

and fun_ (vals : value list) =
  match vals with
  | [ Function _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "fun? expects one argument")

and keys = function
  | [ Map m ] -> List (List.map ~f:(fun (k, _) -> Atom k) @@ MlispMap.pairs m)
  | _ -> assert false

and values = function
  | [ Map m ] -> List (List.map ~f:(fun (_, v) -> v) @@ MlispMap.pairs m)
  | _ -> assert false

and pairs = function
  | [ Map m ] ->
      List (List.map ~f:(fun (k, v) -> ConsCell (Atom k, v)) @@ MlispMap.pairs m)
  | _ -> assert false

and get_map = function
  | [ x; Map m ] -> (
      let res = MlispMap.get m @@ show_value x in
      match res with
      | Some v -> v
      | None -> Atom "nil")
  | _ -> assert false
