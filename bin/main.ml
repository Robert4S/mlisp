open Mlisp
open Core
open! Cases
module F = Fmt

let count_char s c =
  let count acc c2 = if Char.(c2 = c) then acc + 1 else acc in
  let chars = String.to_list s in
  List.fold chars ~init:0 ~f:count

let repl name env () =
  let rec aux buf =
    Out_channel.(flush stdout);
    let buf = String.append buf In_channel.(input_line_exn stdin) in
    let opens = count_char buf '(' in
    let closes = count_char buf ')' in
    if closes >= opens then (
      print_string "=> ";
      Parse.evaluate_program env buf;
      print_string "mlisp ";
      print_string name;
      print_string " > ";
      aux buf)
    else aux buf
  in
  let _ = aux "" in
  ()

let () =
  let args = Sys.get_argv () in
  if Array.length args = 3 then (
    if String.(args.(1) = "-r") then (
      let filename = args.(2) in
      let env = Parse.get_text filename () in
      let name = String.drop_suffix filename 6 in
      print_string "mlisp ";
      print_string name;
      print_string " > ";

      repl name env ())
    else if String.(args.(1) = "-c") then
      let filename = args.(2) in
      let _ = Parse.get_text filename () in
      ())
  else (
    print_string "mlisp ";
    print_string "";
    print_string " > ";
    repl "" (Env.populate ()) ())
