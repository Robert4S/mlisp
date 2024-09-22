open Common_types

type t = type_t [@@deriving eq, show, ord]
type value = type_value [@@deriving eq, show, ord]

val create : string -> string list -> t
val parent : t -> string
val fields : t -> string list
val field_exists : t -> string -> bool
val get : value -> string -> value_t option
val get_type : value -> t
val inner : value -> value_t MMap.t
val construct : t -> value_t MMap.t -> value option

