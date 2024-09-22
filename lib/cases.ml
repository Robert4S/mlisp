open! Core

let env = Eval.remake @@ Prelude.populate (module Eval) ()

let%expect_test "simple expression" =
  let _ = Parse.evaluate_program Eval.eval env "5" in
  [%expect {| 5 |}]

let%expect_test "basic arithemtic" =
  let _ = Parse.evaluate_program Eval.eval env "(* 2 (+ 5 5))" in
  [%expect {| 20 |}]

let%expect_test "printing from lisp" =
  let _ = Parse.evaluate_program Eval.eval env "(io-puts 5)" in
  [%expect {|
    5
    nil
    |}]

let%expect_test "functions" =
  let _ =
    Parse.evaluate_program Eval.eval env
      "(defun make-string (a b c) (str a b c)) (make-string \"Hello\" \" \" 5)"
  in
  [%expect {| ""Hello"" "5" |}]

let%expect_test "recursion" =
  let program =
    "\n\
    \    (defun fact (n) \n\
    \      (cond\n\
    \        (= 0 n) 1\n\
    \        true (* n (fact (- n 1)))))\n\
    \    (fact 5)\n\
    \    "
  in
  let _ = Parse.evaluate_program Eval.eval env program in
  [%expect {| 120 |}]

let%expect_test "variable captures" =
  let program =
    Parse.parse
      "\n\
      \    (defun fact (n)\n\
      \    \n\
      \     (if (= 0 n)\n\n\
      \          1\n\n\
      \          (* n (fact (- n 1)))))\n"
  in
  let bound_vars = Eval.bound_frame [ "n" ] program (Mod.get_env env) in
  Fmt.pr "%s" @@ Env.show Value.pp bound_vars;
  [%expect
    {|
    * : <func>
    - : <func>
    = : <func>
    fact : <func>
    if : <func>
    |}]

let%expect_test "clojure-like maps" =
  let env = Eval.remake @@ Prelude.populate (module Eval) () in
  let program = "(def my-map {:hello \"world\"}) (get :hello my-map)" in
  let _ = Parse.evaluate_program Eval.eval env program in
  [%expect {|"world"|}]

let%expect_test "object syntax" =
  let env = Eval.remake @@ Prelude.populate (module Eval) () in
  let program = "(def my-map {:hello \"world\"}) (:hello my-map)" in
  let _ = Parse.evaluate_program Eval.eval env program in
  [%expect {| "world" |}]

let%expect_test "import syntax" =
  let program = "(import hello)" in
  Fmt.pr "%s" (Ast.show_expr @@ Parse.parse program);
  [%expect {| (List [(List [(Atom "import"); (Atom "hello")])]) |}]

let%expect_test "module access" =
  let program = "(hello.world '())" in
  Fmt.pr "%s" (Ast.show_expr @@ Parse.parse program);
  [%expect
    {| (List [(List [(ModAccess ((Atom "hello"), "world")); (Quoted (List []))])]) |}]

let%expect_test "userdef type instantiation" =
  let program = "#Hello{:world 10}" in
  Fmt.pr "%s" (Ast.show_expr @@ Parse.parse program);
  [%expect {| (List [(TypeCreate ("#Hello", [(Atom ":world"); (Int 10)]))]) |}]

let%expect_test "trait impl" =
  let program =
    "\n\
    \   (defmod h (\n\
    \    (deftype {:hello})))\n\
    \   (deftrait x (\n\
    \    (defun ooga (z))))\n\
    \   (defimpl x h (\n\
    \   (defun ooga (z) 10)))\n\
    \  "
  in
  let _ = Parse.evaluate_program Eval.eval env program in
  [%expect {| nil |}]
