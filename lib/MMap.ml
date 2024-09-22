open Core

module MlispMap : sig
  type 'a t [@@deriving eq, ord]

  val create : (string * 'a) list -> ('a t, string) result
  val get : 'a t -> string -> 'a option
  val pairs : 'a t -> (string * 'a) list
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = 'a String.Map.t [@@deriving eq, ord]

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
    let pairs = Map.to_alist map in
    (match pairs with
    | [] -> ()
    | [ pair ] -> pp_pair formatter pair
    | pair :: pairs ->
        pp_pair formatter pair;
        List.iter pairs ~f:(fun pair ->
            Format.fprintf formatter ", ";
            pp_pair formatter pair));
    Format.fprintf formatter "}"
end
