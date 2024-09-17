open Core
open Ast

exception InvalidArg of string
exception MatchError of string
exception Unbound of string

type 'a gen_func = 'a list -> 'a
and 'a delayed = unit -> 'a
and 'a gen_hashtable = (string, 'a) Hashtbl.t list

type 'a cons_cell = 'a * 'a
and func = { args : string list; body : expr } [@@deriving show, eq, ord]

let compare_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = 0
and equal_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = false
and pp_gen_func _c f (_gf : 'a gen_func) = Format.fprintf f "function"
and compare_delayed _c _a _b = 0
and equal_delayed _c _a_ _b = false
and pp_delayed _c f _gf = Format.fprintf f "function"
and compare_gen_hashtable _c _a _b = 0
and equal_gen_hashtable _c _a _b = false
and pp_gen_hashtable _c f _gf = Format.fprintf f "TABLE"
