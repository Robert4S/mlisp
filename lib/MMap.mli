open Common_types
type 'a t = 'a mlispmap_t [@@deriving show, eq, ord]

val create : (string * 'a) list -> ('a t, string) result
val get : 'a t -> string -> 'a option
val pairs : 'a t -> (string * 'a) list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

