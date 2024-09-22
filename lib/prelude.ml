open Core
open Common_types

let populate (module Eval : EVAL) () =
  let module Builtins = Builtins.Create (Eval) in
  let items = Builtins.items () in
  let tbl =
    [
      Hashtbl.create ~growth_allowed:true
        ~size:(int_of_float (float_of_int (List.length items) *. 1.5))
        (module String);
    ]
  in
  Env.mass_add tbl items;
  Env.of_tbl_list tbl
