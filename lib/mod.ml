open Core
open Ast
open Common_types

type t = mod_t [@@deriving show]

let rec remake ?(t = None) (env : value_t env_t) name = { env; name; t }
and add_type x t = x.t <- Some t
and update { env; _ } key ~f = Env.update (Env.of_tbl_list env) key ~f
and find { env; _ } key = Env.find env key

and make_new (module Eval : EVAL) name =
  let env = Prelude.populate (module Eval) () in
  remake env name

and with_file (module Eval : EVAL) name =
  let contents = In_channel.read_all (name ^ ".mlisp") in
  let expr = Parse.parse contents in
  create ~name:(Some name) (module Eval : EVAL) expr

and create ?(name = None) ?(parent = None) (module Eval : EVAL) expr =
  let mod_ =
    {
      env =
        (match parent with
        | None -> Prelude.populate (module Eval) ()
        | Some e -> get_env e);
      name;
      t = None;
    }
  in
  (match name with
  | Some n -> update mod_ n ~f:(fun _ -> Module mod_)
  | _ -> ());
  match expr with
  | List xs ->
      List.iter xs ~f:(fun e -> Eval.eval mod_ e |> ignore);
      mod_
  | _ -> mod_

and get_env { env; _ } = env
and equal _ _ = false
and compare _ _ = Int.max_value
and name { name; env = _; t = _ } = name
and get_t { t; _ } = t
