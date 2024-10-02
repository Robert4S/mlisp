open Common_types
open Ast

type t = mod_t [@@deriving eq, ord, show]

val with_file : (module EVAL) -> string -> t
val create : 
        ?name:string option -> 
        ?parent:t option -> 
        ?imports:[`Mod of t | `Trait of trait_t] list ->
        ?opens: t list ->
        (module EVAL) ->
        expr ->
        t
val get_env : t -> value_t env_t
val name : t -> string option
(* val remake : ?t:type_t option ref -> value_t env_t -> string option -> [`Mod of t | `Trait of trait_t] list -> t *)
val add_type : t -> Types.t -> unit
val get_t : t -> type_t option 
val make_new :  ?imports:[`Mod of t | `Trait of trait_t] list -> (module EVAL) -> string option -> t
val update : t -> string -> f:(value_t option -> value_t) -> unit
val find : t -> string -> value_t option
val imports : t -> [`Mod of t | `Trait of trait_t] list
val add_import : target:t -> source:[`Mod of t | `Trait of trait_t] -> unit
val add_open : into:t -> expand:t -> unit
val opens : t -> t list
