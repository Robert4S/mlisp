open Common_types

type 'a t = 'a env_t [@@deriving show]

val of_tbl_list : 'a gen_hashtable -> 'a t
val to_tbl_list : 'a t -> 'a gen_hashtable
val find : 'a t -> string -> 'a option
val make : (string * 'a) list -> 'a t
val push : 'a t -> 'a t
val update : 'a t -> string -> f:('a option -> 'a) -> unit
val find_exn : 'a t -> string -> 'a
val push_frame : 'a t -> 'a t -> 'a t
val pairs : 'a t -> (string * 'a) list
val mass_add : 'a t -> (string * 'a) list -> unit
val equal : ('a -> 'a -> bool )-> 'a t -> 'a t -> bool
