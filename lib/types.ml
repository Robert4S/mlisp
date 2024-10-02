open Core
open Common_types

type t = type_t [@@deriving eq, show, ord]
type value = type_value [@@deriving eq, show, ord]

let create parent field_names = { parent; field_names }
let parent { parent; field_names = _ } = parent
let fields { parent = _; field_names } = field_names

let field_exists { parent = _; field_names } field =
  List.mem field_names field ~equal:String.equal

let construct { parent; field_names } map =
  let keys = List.map ~f:(fun (key, _value) -> key) @@ MMap.pairs map in
  if List.for_all keys ~f:(field_exists { parent; field_names }) then
    Some ({ parent; field_names }, map)
  else None

let inner (_, contents) = contents
let get_type (t, _) = t

let get (({ field_names; _ } as _type), contents) field =
  if String.(List.mem field_names field ~equal) then MMap.get contents field
  else None
