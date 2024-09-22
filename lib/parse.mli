open Ast
open Common_types

val parse : string -> expr
val eval_put : eval -> Out_channel.t -> mod_t -> string -> unit
val evaluate_program : eval -> mod_t -> string -> unit
val evaluate_expr : eval -> mod_t -> expr -> unit

