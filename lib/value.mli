open Common_types
type t = value_t [@@deriving eq, ord, show, sexp]

val typeof : t list -> t

