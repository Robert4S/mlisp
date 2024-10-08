open Core
open Ast

let todo _ = failwith "todo"

module StringTable = Hashtbl.Make (String)

let atom_table : string StringTable.t =
  StringTable.create ~growth_allowed:true ~size:10 ()

let intern_atom s =
  match Hashtbl.find atom_table s with
  | Some n -> n
  | None ->
      Hashtbl.set atom_table ~key:s ~data:s;
      s

exception InvalidArg of string
exception MatchError of string
exception Unbound of string

let equal_delayed _ _ _ = false
let compare_delayed _ _ _ = Int.max_value
let equal_gen_func _ _ _ = false
let compare_gen_func _ _ _ = Int.max_value
let equal_gen_hashtable _ _ _ = false
let compare_gen_hashtable _ _ _ = Int.max_value
let pp_gen_hashtable _ formatter _ = Format.fprintf formatter "<Environment>"
let pp_env_t _ ppf _ = Format.fprintf ppf "<Env>"
let pp_delayed _ ppf _ = Format.fprintf ppf "<Thunk>"
let pp_gen_func _ ppf _ = Format.fprintf ppf "<Thunk>"

let pp_mlispmap_t pp_value formatter map =
  let pp_pair formatter (key, value) =
    Format.fprintf formatter "%s = %a" key pp_value value
  in
  Format.fprintf formatter "{";
  (match Map.to_alist map with
  | [] -> ()
  | [ pair ] -> pp_pair formatter pair
  | pair :: pairs ->
      pp_pair formatter pair;
      List.iter pairs ~f:(fun pair ->
          Format.fprintf formatter ", ";
          pp_pair formatter pair));
  Format.fprintf formatter "}"

type 'a gen_hashtable = (string, 'a) Hashtbl.t list
type 'a gen_func = 'a list -> 'a
type 'a env_t = 'a gen_hashtable [@@deriving eq]
type 'a delayed = unit -> 'a

let compare_env_t _ _ _ = Int.max_value

type func = { args : string list; body : expr } [@@deriving show, eq, ord]

type type_t = { parent : string; field_names : string list }
[@@deriving eq, show, ord]

type trait_f = { name : string; args : int } [@@deriving show, eq, ord]

type 'a mlispmap_t = 'a String.Map.t
[@@deriving.show printer pp_mlispmap_t] [@@deriving eq, ord]

type mod_t = {
  env : value_t env_t;
  name : string option;
  mutable opens : mod_t list;
  mutable t : type_t option;
  mutable imports : [ `Mod of mod_t | `Trait of trait_t ] list;
}
[@@deriving eq, ord]

and trait_t = {
  name : string;
  functions : trait_f list;
  impls : (string, mod_t) Hashtbl.t;
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
      [@printer fun ppf _ -> Format.fprintf ppf "<Trait Impls>"]
}
[@@deriving eq, ord]

and value_t =
  | Int of int
  | Float of float
  | Atom of string
  | Function of [ `Userdefined of func * mod_t | `Internal of value_t gen_func ]
  | String of string
  | List of value_t list
  | Thunk of value_t delayed
  | ConsCell of value_t * value_t
  | MMap of value_t mlispmap_t
  | Ref of value_t ref
  | Set of value_t list
  | Module of mod_t
  | UserDefined of type_value
  | Trait of trait_t
[@@deriving eq, ord]

and type_value = type_t * value_t mlispmap_t [@@deriving eq, show, ord]

type eval = mod_t -> expr -> value_t

exception TypeError of value_t * value_t
exception ArgError of value_t * value_t list

module type EVAL = sig
  open Ast

  val eval : mod_t -> expr -> value_t
  val handle_userdef_call : mod_t -> func -> value_t gen_func
  val bound_frame : string list -> expr -> value_t env_t -> value_t env_t
  val funcall : value_t gen_func -> value_t list -> value_t
end

type _ Effect.t += NameError : (mod_t * expr * string) -> value_t Effect.t
