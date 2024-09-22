open Core
open Common_types

type 'a t = 'a mlispmap_t [@@deriving eq, ord]

let create pairs : ('a t, string) result =
  match String.Map.of_alist pairs with
  | `Ok m -> Ok m
  | `Duplicate_key s -> Error s

let get = Map.find
let pairs m = Map.to_alist m

let pp pp_value formatter map =
  let pp_pair formatter (key, value) =
    Format.fprintf formatter "%s = %a" key pp_value value
  in
  Format.fprintf formatter "{";
  (match pairs map with
  | [] -> ()
  | [ pair ] -> pp_pair formatter pair
  | pair :: pairs ->
      pp_pair formatter pair;
      List.iter pairs ~f:(fun pair ->
          Format.fprintf formatter ", ";
          pp_pair formatter pair));
  Format.fprintf formatter "}"

let show pp_v map =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  pp pp_v fmt map;
  Buffer.contents buf
