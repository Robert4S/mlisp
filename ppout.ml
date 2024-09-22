[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    load_path = [];
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    transparent_modules = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies = [("library-name", "mlisp")]
  }]
let () =
  Ppx_expect_runtime.Current_file.set
    ~filename_rel_to_project_root:"lib/common_types.ml"
let () = Ppx_inline_test_lib.set_lib_and_partition "mlisp" "common_types.ml"
open Core
open Ast
exception InvalidArg of string 
exception MatchError of string 
exception Unbound of string 
let equal_delayed _ _ _ = false
let compare_delayed _ _ _ = Int.max_value
let equal_gen_func _ _ _ = false
let compare_gen_func _ _ _ = Int.max_value
let equal_gen_hashtable _ _ _ = false
let compare_gen_hashtable _ _ _ = Int.max_value
let pp_gen_hashtable _ formatter _ = Format.fprintf formatter "<Environment>"
let equal_trait_t _ _ = false
let pp_env_t _ ppf _ = Format.fprintf ppf "<Env>"
let pp_delayed _ ppf _ = Format.fprintf ppf "<Thunk>"
let pp_gen_func _ ppf _ = Format.fprintf ppf "<Thunk>"
type 'a gen_hashtable = (string, 'a) Hashtbl.t list
type 'a gen_func = 'a list -> 'a
type 'a env_t = 'a gen_hashtable[@@deriving eq]
include
  struct
    let _ = fun (_ : 'a env_t) -> ()
    let rec equal_env_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.bool) ->
          'a env_t -> 'a env_t -> Ppx_deriving_runtime.bool
      =
      ((let __0 = equal_gen_hashtable in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_env_t
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a delayed = unit -> 'a
let compare_env_t _ _ _ = Int.max_value
type func = {
  args: string list ;
  body: expr }[@@deriving (show, eq, ord)]
include
  struct
    let _ = fun (_ : func) -> ()
    let rec pp_func :
      Ppx_deriving_runtime.Format.formatter ->
        func -> Ppx_deriving_runtime.unit
      =
      ((let __0 = pp_expr in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun fmt ->
              fun x ->
                Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
                ((Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                    "Common_types.args";
                  ((fun x ->
                      Ppx_deriving_runtime.Format.fprintf fmt "@[<2>[";
                      ignore
                        (List.fold_left
                           (fun sep ->
                              fun x ->
                                if sep
                                then
                                  Ppx_deriving_runtime.Format.fprintf fmt
                                    ";@ ";
                                (Ppx_deriving_runtime.Format.fprintf fmt "%S")
                                  x;
                                true) false x);
                      Ppx_deriving_runtime.Format.fprintf fmt "@,]@]"))
                    x.args;
                  Ppx_deriving_runtime.Format.fprintf fmt "@]");
                 Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                 Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " "body";
                 (__0 fmt) x.body;
                 Ppx_deriving_runtime.Format.fprintf fmt "@]");
                Ppx_deriving_runtime.Format.fprintf fmt "@ }@]")
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_func : func -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_func x[@@ocaml.warning
                                                                    "-32"]
    let _ = pp_func
    and _ = show_func
    let rec equal_func : func -> func -> Ppx_deriving_runtime.bool =
      ((let __0 = equal_expr in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                ((let rec loop x y =
                    match (x, y) with
                    | ([], []) -> true
                    | (a::x, b::y) ->
                        ((fun (a : string) -> fun b -> a = b) a b) &&
                          (loop x y)
                    | _ -> false in
                  fun x -> fun y -> loop x y) lhs.args rhs.args) &&
                  (__0 lhs.body rhs.body))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_func
    let rec compare_func : func -> func -> Ppx_deriving_runtime.int =
      ((let __1 = compare_expr
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (let rec loop x y =
                         match (x, y) with
                         | ([], []) -> 0
                         | ([], _) -> (-1)
                         | (_, []) -> 1
                         | (a::x, b::y) ->
                             (match (__0 ()) a b with
                              | 0 -> loop x y
                              | x -> x) in
                       fun x -> fun y -> loop x y) lhs.args rhs.args
                with
                | 0 -> __1 lhs.body rhs.body
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_func
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type type_t = {
  parent: string ;
  field_names: string list }[@@deriving (eq, show, ord)]
include
  struct
    let _ = fun (_ : type_t) -> ()
    let rec equal_type_t : type_t -> type_t -> Ppx_deriving_runtime.bool =
      ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
          fun lhs ->
            fun rhs ->
              ((fun (a : string) -> fun b -> a = b) lhs.parent rhs.parent) &&
                ((let rec loop x y =
                    match (x, y) with
                    | ([], []) -> true
                    | (a::x, b::y) ->
                        ((fun (a : string) -> fun b -> a = b) a b) &&
                          (loop x y)
                    | _ -> false in
                  fun x -> fun y -> loop x y) lhs.field_names rhs.field_names))
      [@ocaml.warning "-39"][@ocaml.warning "-A"])[@@ocaml.warning "-39"]
    let _ = equal_type_t
    let rec pp_type_t :
      Ppx_deriving_runtime.Format.formatter ->
        type_t -> Ppx_deriving_runtime.unit
      =
      ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
          fun fmt ->
            fun x ->
              Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
              ((Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                  "Common_types.parent";
                (Ppx_deriving_runtime.Format.fprintf fmt "%S") x.parent;
                Ppx_deriving_runtime.Format.fprintf fmt "@]");
               Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
               Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                 "field_names";
               ((fun x ->
                   Ppx_deriving_runtime.Format.fprintf fmt "@[<2>[";
                   ignore
                     (List.fold_left
                        (fun sep ->
                           fun x ->
                             if sep
                             then
                               Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                             (Ppx_deriving_runtime.Format.fprintf fmt "%S") x;
                             true) false x);
                   Ppx_deriving_runtime.Format.fprintf fmt "@,]@]"))
                 x.field_names;
               Ppx_deriving_runtime.Format.fprintf fmt "@]");
              Ppx_deriving_runtime.Format.fprintf fmt "@ }@]")
      [@ocaml.warning "-39"][@ocaml.warning "-A"])
    and show_type_t : type_t -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_type_t x[@@ocaml.warning
                                                                    "-32"]
    let _ = pp_type_t
    and _ = show_type_t
    let rec compare_type_t : type_t -> type_t -> Ppx_deriving_runtime.int =
      ((let __1 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__0 ()) lhs.parent rhs.parent with
                | 0 ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match (__1 ()) a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs.field_names
                      rhs.field_names
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_type_t
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type trait_f = {
  name: string ;
  args: int }[@@deriving (show, eq, ord)]
include
  struct
    let _ = fun (_ : trait_f) -> ()
    let rec pp_trait_f :
      Ppx_deriving_runtime.Format.formatter ->
        trait_f -> Ppx_deriving_runtime.unit
      =
      ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
          fun fmt ->
            fun x ->
              Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
              ((Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                  "Common_types.name";
                (Ppx_deriving_runtime.Format.fprintf fmt "%S") x.name;
                Ppx_deriving_runtime.Format.fprintf fmt "@]");
               Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
               Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " "args";
               (Ppx_deriving_runtime.Format.fprintf fmt "%d") x.args;
               Ppx_deriving_runtime.Format.fprintf fmt "@]");
              Ppx_deriving_runtime.Format.fprintf fmt "@ }@]")
      [@ocaml.warning "-39"][@ocaml.warning "-A"])
    and show_trait_f : trait_f -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_trait_f x
    [@@ocaml.warning "-32"]
    let _ = pp_trait_f
    and _ = show_trait_f
    let rec equal_trait_f : trait_f -> trait_f -> Ppx_deriving_runtime.bool =
      ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
          fun lhs ->
            fun rhs ->
              ((fun (a : string) -> fun b -> a = b) lhs.name rhs.name) &&
                ((fun (a : int) -> fun b -> a = b) lhs.args rhs.args))
      [@ocaml.warning "-39"][@ocaml.warning "-A"])[@@ocaml.warning "-39"]
    let _ = equal_trait_f
    let rec compare_trait_f : trait_f -> trait_f -> Ppx_deriving_runtime.int
      =
      ((let __1 () (a : int) b = Ppx_deriving_runtime.compare a b
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__0 ()) lhs.name rhs.name with
                | 0 -> (__1 ()) lhs.args rhs.args
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_trait_f
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type mod_t =
  {
  env: value_t env_t ;
  name: string option ;
  mutable t: type_t option }[@@deriving.eq equal (fun _ -> fun _ -> false)]
[@@deriving.ord compare (fun _ -> fun _ -> Int.max_value)]
and trait_t =
  {
  name: string ;
  functions: trait_f list ;
  impls: (string, mod_t) Hashtbl.t
    [@equal fun _ -> fun _ -> false][@compare
                                      fun _ -> fun _ -> Int.max_value]
    [@printer fun ppf -> fun _ -> Format.fprintf ppf "<Trait Impls>"]}
[@@deriving eq]
and value_t =
  | Int of int 
  | Float of float 
  | Atom of string 
  | Function of
  [ `Userdefined of (func * value_t gen_hashtable) 
  | `Internal of value_t gen_func ] 
  | String of string 
  | List of value_t list 
  | Thunk of value_t delayed 
  | ConsCell of value_t * value_t 
  | Map of value_t mlispmap_t 
  | Ref of value_t ref 
  | Set of value_t list 
  | Module of mod_t 
  | UserDefined of type_value 
  | Trait of trait_t [@@deriving (eq, ord)]
and 'a mlispmap_t = 'a String.Map.t[@@deriving (eq, ord)][@@deriving.show
                                                           printer
                                                             (fun _ ->
                                                                fun ppf ->
                                                                  fun _ ->
                                                                    Format.fprintf
                                                                    ppf
                                                                    "<Map>")]
and type_value = (type_t * value_t mlispmap_t)[@@deriving (eq, show, ord)]
include
  struct
    let _ = fun (_ : mod_t) -> ()
    let _ = fun (_ : trait_t) -> ()
    let _ = fun (_ : value_t) -> ()
    let _ = fun (_ : 'a mlispmap_t) -> ()
    let _ = fun (_ : type_value) -> ()
    let rec equal_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_type_t
        and __1 = equal_env_t
        and __0 = equal_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((__1 __0) lhs.env rhs.env) &&
                   ((fun x ->
                       fun y ->
                         match (x, y) with
                         | (None, None) -> true
                         | (Some a, Some b) ->
                             ((fun (a : string) -> fun b -> a = b)) a b
                         | _ -> false) lhs.name rhs.name))
                  &&
                  ((fun x ->
                      fun y ->
                        match (x, y) with
                        | (None, None) -> true
                        | (Some a, Some b) -> __2 a b
                        | _ -> false) lhs.t rhs.t))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.bool =
      ((let __1 () _ _ = false
        and __0 = equal_trait_f in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((fun (a : string) -> fun b -> a = b) lhs.name rhs.name) &&
                   ((let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__0 a b) && (loop x y)
                       | _ -> false in
                     fun x -> fun y -> loop x y) lhs.functions rhs.functions))
                  && ((__1 ()) lhs.impls rhs.impls))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_value_t : value_t -> value_t -> Ppx_deriving_runtime.bool =
      ((let __16 = equal_trait_t
        and __15 = equal_type_value
        and __14 = equal_mod_t
        and __13 = equal_value_t
        and __12 = equal_value_t
        and __11 = equal_mlispmap_t
        and __10 = equal_value_t
        and __9 = equal_value_t
        and __8 = equal_value_t
        and __7 = equal_delayed
        and __6 = equal_value_t
        and __5 = equal_value_t
        and __4 = equal_gen_func
        and __3 = equal_value_t
        and __2 = equal_gen_hashtable
        and __1 = equal_value_t
        and __0 = equal_func in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) ->
                    ((fun (a : int) -> fun b -> a = b)) lhs0 rhs0
                | (Float lhs0, Float rhs0) ->
                    ((fun (a : float) -> fun b -> a = b)) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1)))
                                lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__4 __3) lhs rhs
                          | _ -> false)) lhs0 rhs0
                | (String lhs0, String rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__5 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__7 __6) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (__8 lhs0 rhs0) && (__9 lhs1 rhs1)
                | (Map lhs0, Map rhs0) -> (__11 __10) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __12 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__13 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __14 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __15 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __16 lhs0 rhs0
                | _ -> false)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.bool) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.bool
      =
      ((let __0 = String.Map.equal in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_mlispmap_t
        and __1 = equal_value_t
        and __0 = equal_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) -> (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_mod_t
    and _ = equal_trait_t
    and _ = equal_value_t
    and _ = equal_mlispmap_t
    and _ = equal_type_value
    let rec equal_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_type_t
        and __1 = equal_env_t
        and __0 = equal_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((__1 __0) lhs.env rhs.env) &&
                   ((fun x ->
                       fun y ->
                         match (x, y) with
                         | (None, None) -> true
                         | (Some a, Some b) ->
                             ((fun (a : string) -> fun b -> a = b)) a b
                         | _ -> false) lhs.name rhs.name))
                  &&
                  ((fun x ->
                      fun y ->
                        match (x, y) with
                        | (None, None) -> true
                        | (Some a, Some b) -> __2 a b
                        | _ -> false) lhs.t rhs.t))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.bool =
      ((let __1 () _ _ = false
        and __0 = equal_trait_f in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((fun (a : string) -> fun b -> a = b) lhs.name rhs.name) &&
                   ((let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__0 a b) && (loop x y)
                       | _ -> false in
                     fun x -> fun y -> loop x y) lhs.functions rhs.functions))
                  && ((__1 ()) lhs.impls rhs.impls))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_value_t : value_t -> value_t -> Ppx_deriving_runtime.bool =
      ((let __16 = equal_trait_t
        and __15 = equal_type_value
        and __14 = equal_mod_t
        and __13 = equal_value_t
        and __12 = equal_value_t
        and __11 = equal_mlispmap_t
        and __10 = equal_value_t
        and __9 = equal_value_t
        and __8 = equal_value_t
        and __7 = equal_delayed
        and __6 = equal_value_t
        and __5 = equal_value_t
        and __4 = equal_gen_func
        and __3 = equal_value_t
        and __2 = equal_gen_hashtable
        and __1 = equal_value_t
        and __0 = equal_func in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) ->
                    ((fun (a : int) -> fun b -> a = b)) lhs0 rhs0
                | (Float lhs0, Float rhs0) ->
                    ((fun (a : float) -> fun b -> a = b)) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1)))
                                lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__4 __3) lhs rhs
                          | _ -> false)) lhs0 rhs0
                | (String lhs0, String rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__5 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__7 __6) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (__8 lhs0 rhs0) && (__9 lhs1 rhs1)
                | (Map lhs0, Map rhs0) -> (__11 __10) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __12 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__13 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __14 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __15 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __16 lhs0 rhs0
                | _ -> false)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.bool) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.bool
      =
      ((let __0 = String.Map.equal in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_mlispmap_t
        and __1 = equal_value_t
        and __0 = equal_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) -> (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_mod_t
    and _ = equal_trait_t
    and _ = equal_value_t
    and _ = equal_mlispmap_t
    and _ = equal_type_value
    let rec compare_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.int =
      ((let __3 = compare_type_t
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 = compare_env_t
        and __0 = compare_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__1 __0) lhs.env rhs.env with
                | 0 ->
                    (match (fun x ->
                              fun y ->
                                match (x, y) with
                                | (None, None) -> 0
                                | (Some a, Some b) -> (__2 ()) a b
                                | (None, Some _) -> (-1)
                                | (Some _, None) -> 1) lhs.name rhs.name
                     with
                     | 0 ->
                         ((fun x ->
                             fun y ->
                               match (x, y) with
                               | (None, None) -> 0
                               | (Some a, Some b) -> __3 a b
                               | (None, Some _) -> (-1)
                               | (Some _, None) -> 1)) lhs.t rhs.t
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.int =
      ((let __2 () _ _ = Int.max_value
        and __1 = compare_trait_f
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__0 ()) lhs.name rhs.name with
                | 0 ->
                    (match (let rec loop x y =
                              match (x, y) with
                              | ([], []) -> 0
                              | ([], _) -> (-1)
                              | (_, []) -> 1
                              | (a::x, b::y) ->
                                  (match __1 a b with
                                   | 0 -> loop x y
                                   | x -> x) in
                            fun x -> fun y -> loop x y) lhs.functions
                             rhs.functions
                     with
                     | 0 -> (__2 ()) lhs.impls rhs.impls
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_value_t : value_t -> value_t -> Ppx_deriving_runtime.int =
      ((let __20 = compare_trait_t
        and __19 = compare_type_value
        and __18 = compare_mod_t
        and __17 = compare_value_t
        and __16 = compare_value_t
        and __15 = compare_mlispmap_t
        and __14 = compare_value_t
        and __13 = compare_value_t
        and __12 = compare_value_t
        and __11 = compare_delayed
        and __10 = compare_value_t
        and __9 = compare_value_t
        and __8 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __7 = compare_gen_func
        and __6 = compare_value_t
        and __5 = compare_gen_hashtable
        and __4 = compare_value_t
        and __3 = compare_func
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 () (a : float) b = Ppx_deriving_runtime.compare a b
        and __0 () (a : int) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) -> (__0 ()) lhs0 rhs0
                | (Float lhs0, Float rhs0) -> (__1 ()) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) -> (__2 ()) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    match __3 lhs0 rhs0 with
                                    | 0 -> (__5 __4) lhs1 rhs1
                                    | x -> x)) lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__7 __6) lhs rhs
                          | _ ->
                              let to_int =
                                function
                                | `Userdefined _ -> 0
                                | `Internal _ -> 1 in
                              Ppx_deriving_runtime.compare (to_int lhs)
                                (to_int rhs))) lhs0 rhs0
                | (String lhs0, String rhs0) -> (__8 ()) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __9 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__11 __10) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (match __12 lhs0 rhs0 with | 0 -> __13 lhs1 rhs1 | x -> x)
                | (Map lhs0, Map rhs0) -> (__15 __14) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __16 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __17 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __18 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __19 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __20 lhs0 rhs0
                | _ ->
                    let to_int =
                      function
                      | Int _ -> 0
                      | Float _ -> 1
                      | Atom _ -> 2
                      | Function _ -> 3
                      | String _ -> 4
                      | List _ -> 5
                      | Thunk _ -> 6
                      | ConsCell _ -> 7
                      | Map _ -> 8
                      | Ref _ -> 9
                      | Set _ -> 10
                      | Module _ -> 11
                      | UserDefined _ -> 12
                      | Trait _ -> 13 in
                    Ppx_deriving_runtime.compare (to_int lhs) (to_int rhs))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.int) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.int
      =
      ((let __0 = String.Map.compare in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.int =
      ((let __2 = compare_mlispmap_t
        and __1 = compare_value_t
        and __0 = compare_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) ->
                match __0 lhs0 rhs0 with | 0 -> (__2 __1) lhs1 rhs1 | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_mod_t
    and _ = compare_trait_t
    and _ = compare_value_t
    and _ = compare_mlispmap_t
    and _ = compare_type_value
    let rec equal_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_type_t
        and __1 = equal_env_t
        and __0 = equal_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((__1 __0) lhs.env rhs.env) &&
                   ((fun x ->
                       fun y ->
                         match (x, y) with
                         | (None, None) -> true
                         | (Some a, Some b) ->
                             ((fun (a : string) -> fun b -> a = b)) a b
                         | _ -> false) lhs.name rhs.name))
                  &&
                  ((fun x ->
                      fun y ->
                        match (x, y) with
                        | (None, None) -> true
                        | (Some a, Some b) -> __2 a b
                        | _ -> false) lhs.t rhs.t))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.bool =
      ((let __1 () _ _ = false
        and __0 = equal_trait_f in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((fun (a : string) -> fun b -> a = b) lhs.name rhs.name) &&
                   ((let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__0 a b) && (loop x y)
                       | _ -> false in
                     fun x -> fun y -> loop x y) lhs.functions rhs.functions))
                  && ((__1 ()) lhs.impls rhs.impls))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_value_t : value_t -> value_t -> Ppx_deriving_runtime.bool =
      ((let __16 = equal_trait_t
        and __15 = equal_type_value
        and __14 = equal_mod_t
        and __13 = equal_value_t
        and __12 = equal_value_t
        and __11 = equal_mlispmap_t
        and __10 = equal_value_t
        and __9 = equal_value_t
        and __8 = equal_value_t
        and __7 = equal_delayed
        and __6 = equal_value_t
        and __5 = equal_value_t
        and __4 = equal_gen_func
        and __3 = equal_value_t
        and __2 = equal_gen_hashtable
        and __1 = equal_value_t
        and __0 = equal_func in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) ->
                    ((fun (a : int) -> fun b -> a = b)) lhs0 rhs0
                | (Float lhs0, Float rhs0) ->
                    ((fun (a : float) -> fun b -> a = b)) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1)))
                                lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__4 __3) lhs rhs
                          | _ -> false)) lhs0 rhs0
                | (String lhs0, String rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__5 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__7 __6) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (__8 lhs0 rhs0) && (__9 lhs1 rhs1)
                | (Map lhs0, Map rhs0) -> (__11 __10) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __12 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__13 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __14 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __15 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __16 lhs0 rhs0
                | _ -> false)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.bool) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.bool
      =
      ((let __0 = String.Map.equal in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_mlispmap_t
        and __1 = equal_value_t
        and __0 = equal_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) -> (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_mod_t
    and _ = equal_trait_t
    and _ = equal_value_t
    and _ = equal_mlispmap_t
    and _ = equal_type_value
    let rec compare_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.int =
      ((let __3 = compare_type_t
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 = compare_env_t
        and __0 = compare_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__1 __0) lhs.env rhs.env with
                | 0 ->
                    (match (fun x ->
                              fun y ->
                                match (x, y) with
                                | (None, None) -> 0
                                | (Some a, Some b) -> (__2 ()) a b
                                | (None, Some _) -> (-1)
                                | (Some _, None) -> 1) lhs.name rhs.name
                     with
                     | 0 ->
                         ((fun x ->
                             fun y ->
                               match (x, y) with
                               | (None, None) -> 0
                               | (Some a, Some b) -> __3 a b
                               | (None, Some _) -> (-1)
                               | (Some _, None) -> 1)) lhs.t rhs.t
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.int =
      ((let __2 () _ _ = Int.max_value
        and __1 = compare_trait_f
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__0 ()) lhs.name rhs.name with
                | 0 ->
                    (match (let rec loop x y =
                              match (x, y) with
                              | ([], []) -> 0
                              | ([], _) -> (-1)
                              | (_, []) -> 1
                              | (a::x, b::y) ->
                                  (match __1 a b with
                                   | 0 -> loop x y
                                   | x -> x) in
                            fun x -> fun y -> loop x y) lhs.functions
                             rhs.functions
                     with
                     | 0 -> (__2 ()) lhs.impls rhs.impls
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_value_t : value_t -> value_t -> Ppx_deriving_runtime.int =
      ((let __20 = compare_trait_t
        and __19 = compare_type_value
        and __18 = compare_mod_t
        and __17 = compare_value_t
        and __16 = compare_value_t
        and __15 = compare_mlispmap_t
        and __14 = compare_value_t
        and __13 = compare_value_t
        and __12 = compare_value_t
        and __11 = compare_delayed
        and __10 = compare_value_t
        and __9 = compare_value_t
        and __8 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __7 = compare_gen_func
        and __6 = compare_value_t
        and __5 = compare_gen_hashtable
        and __4 = compare_value_t
        and __3 = compare_func
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 () (a : float) b = Ppx_deriving_runtime.compare a b
        and __0 () (a : int) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) -> (__0 ()) lhs0 rhs0
                | (Float lhs0, Float rhs0) -> (__1 ()) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) -> (__2 ()) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    match __3 lhs0 rhs0 with
                                    | 0 -> (__5 __4) lhs1 rhs1
                                    | x -> x)) lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__7 __6) lhs rhs
                          | _ ->
                              let to_int =
                                function
                                | `Userdefined _ -> 0
                                | `Internal _ -> 1 in
                              Ppx_deriving_runtime.compare (to_int lhs)
                                (to_int rhs))) lhs0 rhs0
                | (String lhs0, String rhs0) -> (__8 ()) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __9 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__11 __10) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (match __12 lhs0 rhs0 with | 0 -> __13 lhs1 rhs1 | x -> x)
                | (Map lhs0, Map rhs0) -> (__15 __14) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __16 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __17 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __18 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __19 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __20 lhs0 rhs0
                | _ ->
                    let to_int =
                      function
                      | Int _ -> 0
                      | Float _ -> 1
                      | Atom _ -> 2
                      | Function _ -> 3
                      | String _ -> 4
                      | List _ -> 5
                      | Thunk _ -> 6
                      | ConsCell _ -> 7
                      | Map _ -> 8
                      | Ref _ -> 9
                      | Set _ -> 10
                      | Module _ -> 11
                      | UserDefined _ -> 12
                      | Trait _ -> 13 in
                    Ppx_deriving_runtime.compare (to_int lhs) (to_int rhs))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.int) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.int
      =
      ((let __0 = String.Map.compare in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.int =
      ((let __2 = compare_mlispmap_t
        and __1 = compare_value_t
        and __0 = compare_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) ->
                match __0 lhs0 rhs0 with | 0 -> (__2 __1) lhs1 rhs1 | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_mod_t
    and _ = compare_trait_t
    and _ = compare_value_t
    and _ = compare_mlispmap_t
    and _ = compare_type_value
    let rec equal_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_type_t
        and __1 = equal_env_t
        and __0 = equal_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((__1 __0) lhs.env rhs.env) &&
                   ((fun x ->
                       fun y ->
                         match (x, y) with
                         | (None, None) -> true
                         | (Some a, Some b) ->
                             ((fun (a : string) -> fun b -> a = b)) a b
                         | _ -> false) lhs.name rhs.name))
                  &&
                  ((fun x ->
                      fun y ->
                        match (x, y) with
                        | (None, None) -> true
                        | (Some a, Some b) -> __2 a b
                        | _ -> false) lhs.t rhs.t))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.bool =
      ((let __1 () _ _ = false
        and __0 = equal_trait_f in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                (((fun (a : string) -> fun b -> a = b) lhs.name rhs.name) &&
                   ((let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__0 a b) && (loop x y)
                       | _ -> false in
                     fun x -> fun y -> loop x y) lhs.functions rhs.functions))
                  && ((__1 ()) lhs.impls rhs.impls))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_value_t : value_t -> value_t -> Ppx_deriving_runtime.bool =
      ((let __16 = equal_trait_t
        and __15 = equal_type_value
        and __14 = equal_mod_t
        and __13 = equal_value_t
        and __12 = equal_value_t
        and __11 = equal_mlispmap_t
        and __10 = equal_value_t
        and __9 = equal_value_t
        and __8 = equal_value_t
        and __7 = equal_delayed
        and __6 = equal_value_t
        and __5 = equal_value_t
        and __4 = equal_gen_func
        and __3 = equal_value_t
        and __2 = equal_gen_hashtable
        and __1 = equal_value_t
        and __0 = equal_func in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) ->
                    ((fun (a : int) -> fun b -> a = b)) lhs0 rhs0
                | (Float lhs0, Float rhs0) ->
                    ((fun (a : float) -> fun b -> a = b)) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1)))
                                lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__4 __3) lhs rhs
                          | _ -> false)) lhs0 rhs0
                | (String lhs0, String rhs0) ->
                    ((fun (a : string) -> fun b -> a = b)) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__5 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__7 __6) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (__8 lhs0 rhs0) && (__9 lhs1 rhs1)
                | (Map lhs0, Map rhs0) -> (__11 __10) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __12 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> true
                       | (a::x, b::y) -> (__13 a b) && (loop x y)
                       | _ -> false in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __14 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __15 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __16 lhs0 rhs0
                | _ -> false)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.bool) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.bool
      =
      ((let __0 = String.Map.equal in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and equal_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.bool =
      ((let __2 = equal_mlispmap_t
        and __1 = equal_value_t
        and __0 = equal_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) -> (__0 lhs0 rhs0) && ((__2 __1) lhs1 rhs1))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = equal_mod_t
    and _ = equal_trait_t
    and _ = equal_value_t
    and _ = equal_mlispmap_t
    and _ = equal_type_value
    let rec pp_mod_t :
      Ppx_deriving_runtime.Format.formatter ->
        mod_t -> Ppx_deriving_runtime.unit
      =
      ((let __2 = pp_type_t
        and __1 = pp_env_t
        and __0 = pp_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun fmt ->
              fun x ->
                Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
                (((Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                     "Common_types.env";
                   (__1 (fun fmt -> __0 fmt) fmt) x.env;
                   Ppx_deriving_runtime.Format.fprintf fmt "@]");
                  Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                  Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " "name";
                  ((function
                    | None ->
                        Ppx_deriving_runtime.Format.pp_print_string fmt
                          "None"
                    | Some x ->
                        (Ppx_deriving_runtime.Format.pp_print_string fmt
                           "(Some ";
                         (Ppx_deriving_runtime.Format.fprintf fmt "%S") x;
                         Ppx_deriving_runtime.Format.pp_print_string fmt ")")))
                    x.name;
                  Ppx_deriving_runtime.Format.fprintf fmt "@]");
                 Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                 Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " "t";
                 ((function
                   | None ->
                       Ppx_deriving_runtime.Format.pp_print_string fmt "None"
                   | Some x ->
                       (Ppx_deriving_runtime.Format.pp_print_string fmt
                          "(Some ";
                        (__2 fmt) x;
                        Ppx_deriving_runtime.Format.pp_print_string fmt ")")))
                   x.t;
                 Ppx_deriving_runtime.Format.fprintf fmt "@]");
                Ppx_deriving_runtime.Format.fprintf fmt "@ }@]")
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_mod_t : mod_t -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_mod_t x[@@ocaml.warning
                                                                    "-32"]
    and pp_trait_t :
      Ppx_deriving_runtime.Format.formatter ->
        trait_t -> Ppx_deriving_runtime.unit
      =
      ((let __1 () =
          ((let fprintf = Ppx_deriving_runtime.Format.fprintf in
            fun ppf -> fun _ -> Format.fprintf ppf "<Trait Impls>")
          [@ocaml.warning "-26"])
        and __0 = pp_trait_f in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun fmt ->
              fun x ->
                Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
                (((Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                     "Common_types.name";
                   (Ppx_deriving_runtime.Format.fprintf fmt "%S") x.name;
                   Ppx_deriving_runtime.Format.fprintf fmt "@]");
                  Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                  Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ "
                    "functions";
                  ((fun x ->
                      Ppx_deriving_runtime.Format.fprintf fmt "@[<2>[";
                      ignore
                        (List.fold_left
                           (fun sep ->
                              fun x ->
                                if sep
                                then
                                  Ppx_deriving_runtime.Format.fprintf fmt
                                    ";@ ";
                                (__0 fmt) x;
                                true) false x);
                      Ppx_deriving_runtime.Format.fprintf fmt "@,]@]"))
                    x.functions;
                  Ppx_deriving_runtime.Format.fprintf fmt "@]");
                 Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
                 Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " "impls";
                 ((__1 ()) fmt) x.impls;
                 Ppx_deriving_runtime.Format.fprintf fmt "@]");
                Ppx_deriving_runtime.Format.fprintf fmt "@ }@]")
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_trait_t : trait_t -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_trait_t x
    [@@ocaml.warning "-32"]
    and pp_value_t :
      Ppx_deriving_runtime.Format.formatter ->
        value_t -> Ppx_deriving_runtime.unit
      =
      ((let __16 = pp_trait_t
        and __15 = pp_type_value
        and __14 = pp_mod_t
        and __13 = pp_value_t
        and __12 = pp_value_t
        and __11 = pp_mlispmap_t
        and __10 = pp_value_t
        and __9 = pp_value_t
        and __8 = pp_value_t
        and __7 = pp_delayed
        and __6 = pp_value_t
        and __5 = pp_value_t
        and __4 = pp_gen_func
        and __3 = pp_value_t
        and __2 = pp_gen_hashtable
        and __1 = pp_value_t
        and __0 = pp_func in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun fmt ->
              function
              | Int a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Int@ ";
                   (Ppx_deriving_runtime.Format.fprintf fmt "%d") a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Float a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Float@ ";
                   (Ppx_deriving_runtime.Format.fprintf fmt "%F") a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Atom a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Atom@ ";
                   (Ppx_deriving_runtime.Format.fprintf fmt "%S") a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Function a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Function@ ";
                   ((function
                     | `Userdefined x ->
                         (Ppx_deriving_runtime.Format.fprintf fmt
                            "`Userdefined (@[<hov>";
                          ((fun (a0, a1) ->
                              Ppx_deriving_runtime.Format.fprintf fmt "(@[";
                              ((__0 fmt) a0;
                               Ppx_deriving_runtime.Format.fprintf fmt ",@ ";
                               (__2 (fun fmt -> __1 fmt) fmt) a1);
                              Ppx_deriving_runtime.Format.fprintf fmt "@])"))
                            x;
                          Ppx_deriving_runtime.Format.fprintf fmt "@])")
                     | `Internal x ->
                         (Ppx_deriving_runtime.Format.fprintf fmt
                            "`Internal (@[<hov>";
                          (__4 (fun fmt -> __3 fmt) fmt) x;
                          Ppx_deriving_runtime.Format.fprintf fmt "@])"))) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | String a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.String@ ";
                   (Ppx_deriving_runtime.Format.fprintf fmt "%S") a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | List a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.List@ ";
                   ((fun x ->
                       Ppx_deriving_runtime.Format.fprintf fmt "@[<2>[";
                       ignore
                         (List.fold_left
                            (fun sep ->
                               fun x ->
                                 if sep
                                 then
                                   Ppx_deriving_runtime.Format.fprintf fmt
                                     ";@ ";
                                 (__5 fmt) x;
                                 true) false x);
                       Ppx_deriving_runtime.Format.fprintf fmt "@,]@]")) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Thunk a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Thunk@ ";
                   (__7 (fun fmt -> __6 fmt) fmt) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | ConsCell (a0, a1) ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.ConsCell (@,";
                   ((__8 fmt) a0;
                    Ppx_deriving_runtime.Format.fprintf fmt ",@ ";
                    (__9 fmt) a1);
                   Ppx_deriving_runtime.Format.fprintf fmt "@,))@]")
              | Map a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Map@ ";
                   (__11 (fun fmt -> __10 fmt) fmt) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Ref a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Ref@ ";
                   ((fun x ->
                       Ppx_deriving_runtime.Format.pp_print_string fmt
                         "ref (";
                       (__12 fmt) (!x);
                       Ppx_deriving_runtime.Format.pp_print_string fmt ")"))
                     a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Set a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Set@ ";
                   ((fun x ->
                       Ppx_deriving_runtime.Format.fprintf fmt "@[<2>[";
                       ignore
                         (List.fold_left
                            (fun sep ->
                               fun x ->
                                 if sep
                                 then
                                   Ppx_deriving_runtime.Format.fprintf fmt
                                     ";@ ";
                                 (__13 fmt) x;
                                 true) false x);
                       Ppx_deriving_runtime.Format.fprintf fmt "@,]@]")) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Module a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Module@ ";
                   (__14 fmt) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | UserDefined a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.UserDefined@ ";
                   (__15 fmt) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])")
              | Trait a0 ->
                  (Ppx_deriving_runtime.Format.fprintf fmt
                     "(@[<2>Common_types.Trait@ ";
                   (__16 fmt) a0;
                   Ppx_deriving_runtime.Format.fprintf fmt "@])"))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_value_t : value_t -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_value_t x
    [@@ocaml.warning "-32"]
    and pp_mlispmap_t :
      'a .
        (Ppx_deriving_runtime.Format.formatter ->
           'a -> Ppx_deriving_runtime.unit)
          ->
          Ppx_deriving_runtime.Format.formatter ->
            'a mlispmap_t -> Ppx_deriving_runtime.unit
      =
      ((let __0 = String.Map.pp in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> fun fmt -> __0 (fun fmt -> poly_a fmt) fmt)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_mlispmap_t :
      'a .
        (Ppx_deriving_runtime.Format.formatter ->
           'a -> Ppx_deriving_runtime.unit)
          -> 'a mlispmap_t -> Ppx_deriving_runtime.string
      =
      fun poly_a ->
        fun x ->
          Ppx_deriving_runtime.Format.asprintf "%a" (pp_mlispmap_t poly_a) x
    [@@ocaml.warning "-32"]
    and pp_type_value :
      Ppx_deriving_runtime.Format.formatter ->
        type_value -> Ppx_deriving_runtime.unit
      =
      ((let __2 = pp_mlispmap_t
        and __1 = pp_value_t
        and __0 = pp_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun fmt ->
              fun (a0, a1) ->
                Ppx_deriving_runtime.Format.fprintf fmt "(@[";
                ((__0 fmt) a0;
                 Ppx_deriving_runtime.Format.fprintf fmt ",@ ";
                 (__2 (fun fmt -> __1 fmt) fmt) a1);
                Ppx_deriving_runtime.Format.fprintf fmt "@])")
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])
    and show_type_value : type_value -> Ppx_deriving_runtime.string =
      fun x -> Ppx_deriving_runtime.Format.asprintf "%a" pp_type_value x
    [@@ocaml.warning "-32"]
    let _ = pp_mod_t
    and _ = show_mod_t
    and _ = pp_trait_t
    and _ = show_trait_t
    and _ = pp_value_t
    and _ = show_value_t
    and _ = pp_mlispmap_t
    and _ = show_mlispmap_t
    and _ = pp_type_value
    and _ = show_type_value
    let rec compare_mod_t : mod_t -> mod_t -> Ppx_deriving_runtime.int =
      ((let __3 = compare_type_t
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 = compare_env_t
        and __0 = compare_value_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__1 __0) lhs.env rhs.env with
                | 0 ->
                    (match (fun x ->
                              fun y ->
                                match (x, y) with
                                | (None, None) -> 0
                                | (Some a, Some b) -> (__2 ()) a b
                                | (None, Some _) -> (-1)
                                | (Some _, None) -> 1) lhs.name rhs.name
                     with
                     | 0 ->
                         ((fun x ->
                             fun y ->
                               match (x, y) with
                               | (None, None) -> 0
                               | (Some a, Some b) -> __3 a b
                               | (None, Some _) -> (-1)
                               | (Some _, None) -> 1)) lhs.t rhs.t
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_trait_t : trait_t -> trait_t -> Ppx_deriving_runtime.int =
      ((let __2 () _ _ = Int.max_value
        and __1 = compare_trait_f
        and __0 () (a : string) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (__0 ()) lhs.name rhs.name with
                | 0 ->
                    (match (let rec loop x y =
                              match (x, y) with
                              | ([], []) -> 0
                              | ([], _) -> (-1)
                              | (_, []) -> 1
                              | (a::x, b::y) ->
                                  (match __1 a b with
                                   | 0 -> loop x y
                                   | x -> x) in
                            fun x -> fun y -> loop x y) lhs.functions
                             rhs.functions
                     with
                     | 0 -> (__2 ()) lhs.impls rhs.impls
                     | x -> x)
                | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_value_t : value_t -> value_t -> Ppx_deriving_runtime.int =
      ((let __20 = compare_trait_t
        and __19 = compare_type_value
        and __18 = compare_mod_t
        and __17 = compare_value_t
        and __16 = compare_value_t
        and __15 = compare_mlispmap_t
        and __14 = compare_value_t
        and __13 = compare_value_t
        and __12 = compare_value_t
        and __11 = compare_delayed
        and __10 = compare_value_t
        and __9 = compare_value_t
        and __8 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __7 = compare_gen_func
        and __6 = compare_value_t
        and __5 = compare_gen_hashtable
        and __4 = compare_value_t
        and __3 = compare_func
        and __2 () (a : string) b = Ppx_deriving_runtime.compare a b
        and __1 () (a : float) b = Ppx_deriving_runtime.compare a b
        and __0 () (a : int) b = Ppx_deriving_runtime.compare a b in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun lhs ->
              fun rhs ->
                match (lhs, rhs) with
                | (Int lhs0, Int rhs0) -> (__0 ()) lhs0 rhs0
                | (Float lhs0, Float rhs0) -> (__1 ()) lhs0 rhs0
                | (Atom lhs0, Atom rhs0) -> (__2 ()) lhs0 rhs0
                | (Function lhs0, Function rhs0) ->
                    ((fun lhs ->
                        fun rhs ->
                          match (lhs, rhs) with
                          | (`Userdefined lhs, `Userdefined rhs) ->
                              ((fun (lhs0, lhs1) ->
                                  fun (rhs0, rhs1) ->
                                    match __3 lhs0 rhs0 with
                                    | 0 -> (__5 __4) lhs1 rhs1
                                    | x -> x)) lhs rhs
                          | (`Internal lhs, `Internal rhs) ->
                              (__7 __6) lhs rhs
                          | _ ->
                              let to_int =
                                function
                                | `Userdefined _ -> 0
                                | `Internal _ -> 1 in
                              Ppx_deriving_runtime.compare (to_int lhs)
                                (to_int rhs))) lhs0 rhs0
                | (String lhs0, String rhs0) -> (__8 ()) lhs0 rhs0
                | (List lhs0, List rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __9 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Thunk lhs0, Thunk rhs0) -> (__11 __10) lhs0 rhs0
                | (ConsCell (lhs0, lhs1), ConsCell (rhs0, rhs1)) ->
                    (match __12 lhs0 rhs0 with | 0 -> __13 lhs1 rhs1 | x -> x)
                | (Map lhs0, Map rhs0) -> (__15 __14) lhs0 rhs0
                | (Ref lhs0, Ref rhs0) ->
                    ((fun a -> fun b -> __16 (!a) (!b))) lhs0 rhs0
                | (Set lhs0, Set rhs0) ->
                    (let rec loop x y =
                       match (x, y) with
                       | ([], []) -> 0
                       | ([], _) -> (-1)
                       | (_, []) -> 1
                       | (a::x, b::y) ->
                           (match __17 a b with | 0 -> loop x y | x -> x) in
                     (fun x -> fun y -> loop x y)) lhs0 rhs0
                | (Module lhs0, Module rhs0) -> __18 lhs0 rhs0
                | (UserDefined lhs0, UserDefined rhs0) -> __19 lhs0 rhs0
                | (Trait lhs0, Trait rhs0) -> __20 lhs0 rhs0
                | _ ->
                    let to_int =
                      function
                      | Int _ -> 0
                      | Float _ -> 1
                      | Atom _ -> 2
                      | Function _ -> 3
                      | String _ -> 4
                      | List _ -> 5
                      | Thunk _ -> 6
                      | ConsCell _ -> 7
                      | Map _ -> 8
                      | Ref _ -> 9
                      | Set _ -> 10
                      | Module _ -> 11
                      | UserDefined _ -> 12
                      | Trait _ -> 13 in
                    Ppx_deriving_runtime.compare (to_int lhs) (to_int rhs))
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_mlispmap_t :
      'a .
        ('a -> 'a -> Ppx_deriving_runtime.int) ->
          'a mlispmap_t -> 'a mlispmap_t -> Ppx_deriving_runtime.int
      =
      ((let __0 = String.Map.compare in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun poly_a -> __0 poly_a)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    and compare_type_value :
      type_value -> type_value -> Ppx_deriving_runtime.int =
      ((let __2 = compare_mlispmap_t
        and __1 = compare_value_t
        and __0 = compare_type_t in
        ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
            fun (lhs0, lhs1) ->
              fun (rhs0, rhs1) ->
                match __0 lhs0 rhs0 with | 0 -> (__2 __1) lhs1 rhs1 | x -> x)
          [@ocaml.warning "-A"]))
      [@ocaml.warning "-39"])[@@ocaml.warning "-39"]
    let _ = compare_mod_t
    and _ = compare_trait_t
    and _ = compare_value_t
    and _ = compare_mlispmap_t
    and _ = compare_type_value
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let compare_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = 0
and equal_gen_func _c (_a : 'a gen_func) (_b : 'a gen_func) = false
and pp_gen_func _c f (_gf : 'a gen_func) = Format.fprintf f "function"
and compare_delayed _c _a _b = 0
and equal_delayed _c _a_ _b = false
and pp_delayed _c f _gf = Format.fprintf f "function"
and compare_gen_hashtable _c _a _b = 0
and equal_gen_hashtable _c _a _b = false
and pp_gen_hashtable _c f _gf = Format.fprintf f "TABLE"
let () = Ppx_inline_test_lib.unset_lib "mlisp"
let () = Ppx_expect_runtime.Current_file.unset ()
