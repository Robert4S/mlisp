open Mlisp
open Core
module F = Fmt

let count_char s c =
  let count acc c2 = if Char.(c2 = c) then acc + 1 else acc in
  let chars = String.to_list s in
  List.fold chars ~init:0 ~f:count

let repl name env () =
  let rec aux buf =
    print_string "mlisp ";
    print_string name;
    print_string " > ";
    Out_channel.(flush stdout);
    let buf = String.append buf In_channel.(input_line_exn stdin) in
    let opens = count_char buf '(' in
    let closes = count_char buf ')' in
    if closes >= opens then (
      Parse.evaluate_program env buf;
      aux buf)
    else aux buf
  in
  let _ = aux "" in
  ()

let () =
  let args = Sys.get_argv () in
  if Array.length args = 3 then (
    if String.(args.(1) = "-r") then
      let filename = args.(2) in
      let env = Parse.get_text filename () in
      repl (String.drop_suffix filename 6) env ()
    else if String.(args.(1) = "-c") then
      let filename = args.(2) in
      let _ = Parse.get_text filename () in
      ())
  else repl "" (Env.populate ()) ()
