open Common_types
open Ast
val eval : mod_t -> expr -> value_t
val handle_userdef_call : mod_t -> func -> value_t gen_func
val bound_frame : string list -> expr -> value_t env_t -> value_t env_t
val remake : ?name:string option -> value_t env_t -> mod_t
val funcall : value_t gen_func -> value_t list -> value_t

