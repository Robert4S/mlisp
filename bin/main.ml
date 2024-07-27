open Mlisp
open Core
open! Cases
module F = Fmt

let count_char s c =
  let count acc c2 = if Char.(c2 = c) then acc + 1 else acc in
  let chars = String.to_list s in
  List.fold chars ~init:0 ~f:count

let repl name env () =
  let open Option.Let_syntax in
  let rec aux prompt buf =
    let%bind inp = LNoise.linenoise prompt in
    let%bind inp = (fun n -> if String.(n = "quit") then None else Some n) inp in
    LNoise.history_add inp |> ignore;
    LNoise.history_save ~filename:"history.txt" |> ignore;
    let newbuf = String.append buf inp in
    let opens = count_char newbuf '(' in
    let closes = count_char newbuf ')' in
    try
      if closes >= opens then (
        print_string "=> ";
        Parse.evaluate_program env newbuf;
        Out_channel.(flush stdout);
        aux prompt "")
      else aux prompt newbuf
    with
    | e ->
        print_endline "Error: ";
        print_endline @@ Exn.to_string e;
        Out_channel.(flush stdout);
        aux prompt buf
  in
  LNoise.catch_break false;
  LNoise.history_load ~filename:"history.txt" |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
  let prompt = sprintf "mlisp %s > " name in
  let _ = aux prompt "" in
  ()

let () =
  let args = Sys.get_argv () in
  if Array.length args = 3 then (
    if String.(args.(1) = "-r") then (
      let filename = args.(2) in
      let env = Parse.get_text Out_channel.stdout filename () in
      let name = String.drop_suffix filename 6 in
      Out_channel.(flush stdout);
      repl name env ())
    else if String.(args.(1) = "-c") then
      let filename = args.(2) in
      let _ = Parse.get_text (Out_channel.create "/dev/null") filename () in
      ())
  else repl "" (Potentially.populate ()) ()
