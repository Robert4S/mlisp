open Core
open Ast

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
let equal_trait_t _ _ = false
let pp_env_t _ ppf _ = Format.fprintf ppf "<Env>"
let pp_delayed _ ppf _ = Format.fprintf ppf "<Thunk>"
let pp_gen_func _ ppf _ = Format.fprintf ppf "<Thunk>"

type 'a gen_hashtable = (string, 'a) Hashtbl.t list
type 'a gen_func = 'a list -> 'a
type 'a env_t = 'a gen_hashtable [@@deriving eq]
type 'a delayed = unit -> 'a

let compare_env_t _ _ _ = Int.max_value

type func = { args : string list; body : expr } [@@deriving show, eq, ord]

type type_t = { parent : string; field_names : string list }
[@@deriving eq, show, ord]

type trait_f = { name : string; args : int } [@@deriving show, eq, ord]

type mod_t = { env : value_t env_t; name : string option; mutable t : type_t option }
[@@deriving.eq equal (fun _ _ -> false)]
[@@deriving.ord compare (fun _ _ -> Int.max_value)]

and trait_t = {
  name : string;
  functions : trait_f list;
  impls : (string, mod_t) Hashtbl.t;
      [@equal fun _ _ -> false]
      [@compare fun _ _ -> Int.max_value]
      [@printer fun ppf _ -> Format.fprintf ppf "<Trait Impls>"]
}
[@@deriving eq]

and value_t =
  | Int of int
  | Float of float
  | Atom of string
  | Function of
      [ `Userdefined of func * value_t gen_hashtable | `Internal of value_t gen_func ]
  | String of string
  | List of value_t list
  | Thunk of value_t delayed
  | ConsCell of value_t * value_t
  | Map of value_t mlispmap_t
  | Ref of value_t ref
  | Set of value_t list
  | Module of mod_t
  | UserDefined of type_value
  | Trait of trait_t
[@@deriving eq, ord]

and 'a mlispmap_t = 'a String.Map.t
[@@deriving.show printer (fun _ ppf _ -> Format.fprintf ppf "<Map>")]
[@@deriving eq, ord]

and type_value = type_t * value_t mlispmap_t [@@deriving eq, show, ord]

(* let equal_gen_func _ formatter _ = Format.fprintf formatter "<Function>" *)
let compare_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = 0
and equal_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = false
and pp_gen_func _c f (_gf : 'a gen_func) = Format.fprintf f "function"
and compare_delayed _c _a _b = 0
and equal_delayed _c _a_ _b = false
and pp_delayed _c f _gf = Format.fprintf f "function"
and compare_gen_hashtable _c _a _b = 0
and equal_gen_hashtable _c _a _b = false
and pp_gen_hashtable _c f _gf = Format.fprintf f "TABLE"
