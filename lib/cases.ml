open! Core

let env = Potentially.populate ()

let%expect_test "simple expression" =
  let _ = Parse.evaluate_program env "5" in
  [%expect {| 5 |}]

let%expect_test "basic arithemtic" =
  let _ = Parse.evaluate_program env "(* 2 (+ 5 5))" in
  [%expect {| 20 |}]

let%expect_test "printing from lisp" =
  let _ = Parse.evaluate_program env "(io-puts 5)" in
  [%expect {|
    5
    nil
    |}]

let%expect_test "functions" =
  let _ =
    Parse.evaluate_program env
      "(defun make-string (a b c) (str a b c)) (make-string \"Hello\" \" \" 5)"
  in
  [%expect {| Hello 5 |}]

let%expect_test "recursion" =
  let program =
    "\n\
    \    (defun fact (n) \n\
    \      (cond\n\
    \        (= 0 n) 1\n\
    \        :else (* n (fact (- n 1)))))\n\
    \    (fact 5)\n\
    \    "
  in
  let _ = Parse.evaluate_program env program in
  [%expect {| 120 |}]
