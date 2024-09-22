open Core
open Common_types

type 'a t = 'a env_t

let rec pairs x =
  let x = to_tbl_list x in
  let keys = List.bind x ~f:Hashtbl.keys |> List.sort ~compare:String.compare in
  let values = List.filter_map keys ~f:(find x) in
  List.zip_exn keys values

and of_tbl_list (x : 'a gen_hashtable) : 'a t = x
and to_tbl_list (x : 'a t) : 'a gen_hashtable = x
and push_frame env frames = frames @ env

and update e k ~f =
  let head = List.hd_exn e in
  Hashtbl.update head k ~f

and push e =
  let symbol_table =
    Hashtbl.create ~growth_allowed:true ~size:20 (module String)
  in
  symbol_table :: e

and show pp_val (e : 'a t) =
  let buf = Buffer.create 200 in
  let formatter = Format.formatter_of_buffer buf in
  pp pp_val formatter e;
  Format.pp_print_flush formatter ();
  Buffer.contents buf

and equal _ _ _ = false

and pp pp_val (formatter : Format.formatter) e =
  let zipped = pairs e in
  List.iter zipped ~f:(fun (key, value) ->
      Format.fprintf formatter "%s : " key;
      Format.fprintf formatter "%a\n" pp_val value)

and find e name =
  match e with
  | [] -> None
  | x :: xs -> (
      let found = Hashtbl.find x name in
      match found with Some item -> Some item | None -> find xs name)

and find_exn e name =
  match find e name with None -> raise (Unbound name) | Some v -> v

and mass_add env lst =
  let head = List.hd_exn env in
  match lst with
  | [] -> ()
  | (key, data) :: xs ->
      Hashtbl.add_exn head ~key ~data;
      mass_add env xs

and make pairs =
  let tbl =
    [
      Hashtbl.create ~growth_allowed:true
        ~size:(int_of_float (float_of_int (List.length pairs) *. 1.5))
        (module String);
    ]
  in
  mass_add tbl pairs;
  tbl

let populate () =
  let items = (*Builtins.items*) todo () in
  let tbl =
    [
      Hashtbl.create ~growth_allowed:true
        ~size:(int_of_float (float_of_int (List.length items) *. 1.5))
        (module String);
    ]
  in
  mass_add tbl items;
  of_tbl_list tbl
