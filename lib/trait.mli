open Common_types

type f = trait_f [@@deriving show, eq, ord]
type t = trait_t [@@deriving eq, ord]

val make_f : string -> int -> f
val make_trait : string -> f list -> t
val add_implementer : t -> string -> mod_t -> unit
val to_mod : (module EVAL) -> t -> mod_t
val has_implementer : t -> string -> bool
val impls : t -> (string * mod_t) list

