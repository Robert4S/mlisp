open Core
open Ast
open Common_types

type t = mod_t [@@deriving show]

let rec remake ?(t = None) (env : value_t env_t) name = { env; name; t }
and add_type x t = x.t <- Some t
and update { env; _ } key ~f = Env.update (Env.of_tbl_list env) key ~f
and find { env; _ } key = Env.find env key

(*
and merge m1 m2 name t =
  let env = Env.of_tbl_list [] in
  let pairs1 = Env.pairs m1.env in
  let pairs2 = Env.pairs m2.env in
  let duplicates =
    List.filter_map pairs1 ~f:(fun (key, value) ->
        match List.find pairs2 ~f:(fun (key2, _) -> String.(key2 = key)) with
        | Some (_, value2) -> Some (key, value, value2)
        | None -> None)
  in
  let duplicates =
    List.map duplicates ~f:(fun (key, val1, val2) ->
        match (val1, val2) with
        | Trait t1, Trait t2 ->
            let t2_impls =
              Trait.impls t2
              |> List.filter ~f:(fun (name, _) -> Trait.has_implementer t1 name)
            in
            List.iter t2_impls ~f:(fun (name, impl) ->
                Trait.add_implementer t1 name impl);
            (key, Value.Trait t1)
        | other, _ -> (key, other))
  in
  let find_non_dups l1 l2 =
    List.filter l1 ~f:(fun (key, _) ->
        not (List.exists l2 ~f:(fun (key2, _) -> String.(key = key2))))
  in
  let non_dups = find_non_dups pairs1 pairs2 @ find_non_dups pairs2 pairs1 in
  Env.mass_add env (duplicates @ non_dups);
  remake ~t env name

*)
and make_new name =
  let env = Env.populate () in
  remake env name

and with_file eval name =
  let contents = In_channel.read_all (name ^ ".mlisp") in
  let expr = Parse.parse contents in
  create ~name:(Some name) eval expr

and create ?(name = None) ?(parent = None) (eval : eval) expr =
  let mod_ =
    {
      env = (match parent with None -> Env.populate () | Some e -> get_env e);
      name;
      t = None;
    }
  in
  (match name with
  | Some n -> update mod_ n ~f:(fun _ -> Module mod_)
  | _ -> ());
  match expr with
  | List xs ->
      List.iter xs ~f:(fun e -> eval mod_ e |> ignore);
      mod_
  | _ -> mod_

and get_env { env; _ } = env
and equal _ _ = false
and compare _ _ = Int.max_value
and name { name; env = _; t = _ } = name
and get_t { t; _ } = t
