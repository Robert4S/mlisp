open Common_types
open Ast

type t = mod_t [@@deriving eq, ord, show]

val with_file : eval -> string -> t
val create : ?name:string option -> ?parent:t option -> eval -> expr -> t
val get_env : t -> value_t env_t
val name : t -> string option
val remake : ?t:Types.t option -> value_t env_t -> string option -> t
val add_type : t -> Types.t -> unit
val get_t : t -> Types.t option
val make_new : string option -> t
val update : t -> string -> f:(value_t option -> value_t) -> unit
val find : t -> string -> value_t option
