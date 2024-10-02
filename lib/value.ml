open Core
open Common_types

type t = value_t [@@deriving ord]

let equal v1 v2 =
  match (v1, v2) with
  | Atom a, Atom b -> phys_equal a b
  | other1, other2 -> equal_value_t other1 other2

let rec pp (formatter : Format.formatter) value =
  match value with
  | Int i -> Format.fprintf formatter "%d" i
  | Float f -> Format.fprintf formatter "%f" f
  | Atom a -> Format.fprintf formatter "%s" a
  | Function _ -> Format.fprintf formatter "<func>"
  | String s -> Format.fprintf formatter "\"%s\"" s
  | List vals ->
      List.iter vals ~f:(fun value -> Format.fprintf formatter "%a " pp value)
  | Thunk _ -> Format.fprintf formatter "<thunk>"
  | ConsCell (car, cdr) -> Format.fprintf formatter "%a . %a" pp car pp cdr
  | MMap m -> MMap.pp pp formatter m
  | Ref x -> Format.fprintf formatter "Ref(%a)" pp !x
  | Set xs ->
      Format.fprintf formatter "#{";
      (match xs with
      | first :: rest ->
          Format.fprintf formatter "%a" pp first;
          List.iter rest ~f:(fun value -> Format.fprintf formatter " %a" pp value)
      | _ -> ());
      Format.fprintf formatter "}"
  | Module { name = Some name; t = Some t; _ } ->
      Format.fprintf formatter "<Module %s with type {%s}>" name (show_type_t t)
  | Module { name = Some name; _ } -> Format.fprintf formatter "<Module %s>" name
  | Module _ -> Format.fprintf formatter "<Module>"
  | UserDefined u ->
      let name = Types.parent @@ Types.get_type u in
      let inner = Types.inner u in
      Format.fprintf formatter "#%s" name;
      MMap.pp pp formatter inner
  | Trait { name; _ } -> Format.fprintf formatter "<Trait %s>" name

and typeof (vals : t list) : t =
  let rec aux vals acc : string =
    match vals with
    | [] -> acc
    | x :: xs ->
        let curr =
          match x with
          | String _ -> "String "
          | Atom a -> sprintf "Atom %s " a
          | Function _ -> "Function "
          | Int _ -> "Int "
          | Float _ -> "Float "
          | Thunk _ -> "Thunk "
          | List _ -> "List "
          | ConsCell (a, b) ->
              sprintf "ConsCell (%s) (%s) " (aux [ a ] "") (aux [ b ] "")
          | MMap _ -> "Map "
          | Ref r ->
              "Ref "
              ^ (function
                  | String s -> s
                  | _ -> assert false)
              @@ typeof [ !r ]
          | Set _ -> "Set "
          | Module _ -> "Module "
          | UserDefined x -> Types.parent (Types.get_type x) ^ " "
          | Trait _ -> "Trait "
        in
        let acc = String.append acc curr in
        aux xs acc
  in
  String (aux vals "")

let show value = Format.asprintf "%a" pp value
let sexp_of_t = sexp_of_opaque
let t_of_sexp = opaque_of_sexp
