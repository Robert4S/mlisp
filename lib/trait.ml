open Core
open Common_types

type f = trait_f [@@deriving show, eq, ord]
type t = trait_t

let equal _ _ = false
and compare _ _ = Int.max_value
and make_f name args = { name; args }
and has_implementer { impls; _ } name = Hashtbl.find impls name |> Option.is_some

and impls { impls; _ } =
  let keys = Hashtbl.keys impls in
  let vals = List.filter_map keys ~f:(Hashtbl.find impls) in
  List.zip_exn keys vals

and make_trait name functions =
  {
    name;
    functions;
    impls = (Hashtbl.create ~growth_allowed:true ~size:10) (module String);
  }

let add_implementer x name i = Hashtbl.update x.impls name ~f:(fun _ -> i)

let to_mod (module Eval : EVAL) { functions; impls; name } =
  let get_impl (func : f) args =
    Hashtbl.keys impls |> List.iter ~f:print_endline;
    match args with
    | [] -> assert false
    | UserDefined t :: _ -> (
        let value_type = Types.get_type t in
        match Hashtbl.find impls (Types.parent value_type) with
        | Some m -> (
            match Env.find (Mod.get_env m) func.name with
            | Some (Function (`Userdefined (func, env))) ->
                let f =
                  Eval.handle_userdef_call
                    (Mod.remake (Env.of_tbl_list env) None)
                    func
                  |> Eval.funcall
                in
                f args
            | _ ->
                failwithf "trait %s is not implemented for %s" name
                  (Types.parent value_type) ())
        | _ -> failwithf "module %s found" (Types.parent value_type) ())
    | _ ->
        failwith "first argument of trait function must be a module associated type"
  in
  let new_functions =
    functions |> List.map ~f:(fun { name; args } -> (name, get_impl { name; args }))
  in
  let new_mod = Mod.make_new (module Eval) (Some name) in
  List.iter new_functions ~f:(fun (name, fn) ->
      Env.update (Mod.get_env new_mod) name ~f:(fun _ -> Function (`Internal fn)));
  new_mod
