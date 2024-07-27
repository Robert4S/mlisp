open Core
module F = Fmt

type 'a gen_func = 'a list -> 'a

let compare_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = 0
let equal_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = false
let pp_gen_func _c f (_gf : 'a gen_func) = Format.fprintf f "function"

type 'a delayed = unit -> 'a
type 'a cons_cell = 'a * 'a [@@deriving show, eq, ord]

let compare_delayed _c _a _b = 0
let equal_delayed _c _a_ _b = false
let pp_delayed _c f _gf = Format.fprintf f "function"

type func = { args : string list; body : Ast.expr } [@@deriving show, eq, ord]
type 'a gen_hashtable = (string, 'a) Hashtbl.t list

let compare_gen_hashtable _c _a _b = 0
let equal_gen_hashtable _c _a _b = false
let pp_gen_hashtable _c f _gf = Format.fprintf f "TABLE"

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
[@@deriving eq, ord]

let list_to_cons_cell (ls : value list) =
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

let rec is_valid_list (value : value) =
  match value with
  | Atom "nil" -> true
  | ConsCell (_, rest) -> is_valid_list rest
  | _ -> false

let rec show_value value =
  match value with
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | Atom a -> a
  | Function _ -> "<func>"
  | String s -> s
  | List vals -> String.concat ~sep:" " @@ List.map vals ~f:show_value
  | Thunk _ -> "<thunk>"
  | ConsCell (car, cdr) -> Format.sprintf "%s . %s" (show_value car) (show_value cdr)

let fake_func = Function (`Internal (fun _ -> failwith "not a real function"))
let bool_to_atom a = if a then Atom "true" else Atom "false"

type env = (string, value) Hashtbl.t list

let update e k ~f =
  let head = List.hd_exn e in
  Hashtbl.update head k ~f

let push e =
  let symbol_table = Hashtbl.create ~growth_allowed:true ~size:20 (module String) in
  symbol_table :: e

exception Unbound of string

let show (e : env) =
  let open List.Let_syntax in
  let keys = e >>= Hashtbl.keys in
  let data = e >>= Hashtbl.data >>| show_value in
  String.concat ~sep:" "
  @@ List.map ~f:(fun (a, b) -> sprintf "(%s : %s)" a b)
  @@ List.zip_exn keys data

let rec find e name =
  match e with
  | [] ->
      F.pr "%s\n" @@ show e;
      raise (Unbound name)
  | x :: xs -> (
      let found = Hashtbl.find x name in
      match found with
      | Some item -> item
      | None -> find xs name)

let rec mass_add env lst =
  let head = List.hd_exn env in
  match lst with
  | [] -> ()
  | (key, data) :: xs ->
      Hashtbl.add_exn head ~key ~data;
      mass_add env xs

exception TypeError of value * value

let bin_add a b =
  match (a, b) with
  | Int a, Int b -> Int (a + b)
  | Float a, Float b -> Float (a +. b)
  | Int a, Float b -> Float (b +. float_of_int a)
  | Float a, Int b -> Float (a +. float_of_int b)
  | String a, String b -> String (String.append a b)
  | _ -> raise (TypeError (a, b))

let bin_minus a b =
  match (a, b) with
  | Int a, Int b -> Int (a - b)
  | Float a, Float b -> Float (a -. b)
  | Int a, Float b -> Float (float_of_int a -. b)
  | Float a, Int b -> Float (a -. float_of_int b)
  | _ ->
      print_endline @@ show_value a;
      raise (TypeError (a, b))

let bin_mul a b =
  match (a, b) with
  | Int a, Int b -> Int (a * b)
  | Float a, Float b -> Float (a *. b)
  | Int a, Float b -> Float (float_of_int a *. b)
  | Float a, Int b -> Float (a *. float_of_int b)
  | _ -> raise (TypeError (a, b))

let bin_div a b =
  match (a, b) with
  | Int a, Int b -> Int (a / b)
  | Float a, Float b -> Float (a /. b)
  | Int a, Float b -> Float (float_of_int a /. b)
  | Float a, Int b -> Float (a /. float_of_int b)
  | _ -> raise (TypeError (a, b))

let bin_gt a b =
  match (a, b) with
  | Int a, Int b -> bool_to_atom (a > b)
  | Float a, Float b -> bool_to_atom Float.(a > b)
  | Int a, Float b -> bool_to_atom Float.(float_of_int a > b)
  | Float a, Int b -> bool_to_atom Float.(a > float_of_int b)
  | _ -> raise (TypeError (a, b))

let bin_lt a b =
  match (a, b) with
  | Int a, Int b -> bool_to_atom (a < b)
  | Float a, Float b -> bool_to_atom Float.(a < b)
  | Int a, Float b -> bool_to_atom Float.(float_of_int a < b)
  | Float a, Int b -> bool_to_atom Float.(a < float_of_int b)
  | _ -> raise (TypeError (a, b))

let bin_eq = equal_value

let bin_gte a b =
  match bin_gt a b with
  | Atom "true" -> Atom "true"
  | Atom "false" -> bool_to_atom @@ equal_value a b
  | other -> failwith (show_value other)

let bin_lte a b =
  match bin_lt a b with
  | Atom "true" -> Atom "true"
  | Atom "false" -> bool_to_atom @@ equal_value a b
  | other -> failwith (show_value other)

let bin_mod a b =
  match (a, b) with
  | Int a, Int b -> Int (a mod b)
  | _ -> raise (TypeError (a, b))

exception ArgError of value * value list

let str vals =
  String
    (List.fold vals ~init:"" ~f:(fun acc value ->
         String.append acc @@ show_value value))

let io_puts value =
  print_endline @@ show_value @@ List.hd_exn value;
  Atom "nil"

let eq (vals : value list) : value =
  bool_to_atom @@ Option.is_some @@ List.all_equal ~equal:equal_value vals

let rec add (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_add in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal add), vals))

let rec sub (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_minus in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal sub), vals))

let rec mul (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_mul in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal mul), vals))

let rec div (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_div in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal div), vals))

let rec gt (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_gt in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal gt), vals))

let rec lt (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_lt in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal lt), vals))

let rec gte (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_gte in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal gte), vals))

let rec lte (vals : value list) : value =
  let reduced = List.reduce vals ~f:bin_lte in
  match reduced with
  | Some o -> o
  | None -> raise (ArgError (Function (`Internal lte), vals))

let rec if_ vals =
  match vals with
  | [ Thunk pred; Thunk succ; Thunk els ] -> if_ [ pred (); Thunk succ; Thunk els ]
  | [ Atom "true"; Thunk succ; _ ] -> succ ()
  | [ Atom "false"; _; Thunk els ] -> els ()
  | _ -> raise (ArgError (Function (`Internal if_), vals))

let rec mod_ vals =
  match vals with
  | [ a; b ] -> bin_mod a b
  | _ -> raise (ArgError (Function (`Internal mod_), vals))

exception InvalidArg of string
exception MatchError of string

let zip_pairs lst =
  let rec aux lst acc =
    match lst with
    | [] -> List.rev acc
    | a :: b :: xs -> aux xs ((a, b) :: acc)
    | _ -> failwith "invalid"
  in
  aux lst []

let cond (vals : value list) : value =
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

let print_arg_list vals =
  List.iter vals ~f:(fun arg -> print_endline @@ show_value arg)

let nil (vals : value list) =
  match vals with
  | [ List [] ] -> Atom "true"
  | [ Atom "nil" ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "nil? expects 1 argument")

let list (vals : value list) =
  match vals with
  | [ List _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "list? expects 1 argument")

let cell (vals : value list) =
  match vals with
  | [ ConsCell _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "list? expects 1 argument")

let cons (vals : value list) =
  match vals with
  | [ x; List ls ] -> List (x :: ls)
  | [ x; Atom "nil" ] -> List [ x ]
  | [ x; y ] -> ConsCell (x, y)
  | _ -> raise (InvalidArg "cons expects a value and a list")

let car (vals : value list) =
  match vals with
  | [ ConsCell (car, _cdr) ] -> car
  | _ -> raise (InvalidArg "cannot get the car off of a non cons list")

let cdr (vals : value list) =
  match vals with
  | [ ConsCell (_car, cdr) ] -> cdr
  | _ -> raise (InvalidArg "cannot get the cdr off of a non cons list")

let fun_ (vals : value list) =
  match vals with
  | [ Function _ ] -> Atom "true"
  | [ _ ] -> Atom "false"
  | _ -> raise (InvalidArg "fun? expects one argument")
