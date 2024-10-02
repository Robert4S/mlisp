open Mlisp
open Core
open Common_types
open! Cases
module F = Fmt

let mode_param =
  let open Command.Param in
  flag "-m" (optional string) ~doc:"mode for the repl, can be either repl or compile"

let files_param =
  let open Command.Param in
  flag "-f" (listed string)
    ~doc:"files to include in running, like the files passed to ocamlc or ocamlopt"

let count_char s c =
  let count acc c2 = if Char.(c2 = c) then acc + 1 else acc in
  let chars = String.to_list s in
  List.fold chars ~init:0 ~f:count

let repl name env () =
  let open Option.Let_syntax in
  let rec aux prompt buf =
    let%bind inp = LNoise.linenoise prompt in
    let%bind inp = (fun n -> if String.(n = ":q") then None else Some n) inp in
    LNoise.history_add inp |> ignore;
    LNoise.history_save ~filename:"history.txt" |> ignore;
    let newbuf = String.append buf inp in
    let opens = count_char newbuf '(' in
    let closes = count_char newbuf ')' in
    try
      if closes >= opens then (
        let open Effect.Deep in
        print_string "=> ";
        match_with
          (fun () -> Parse.evaluate_program Eval.eval env newbuf)
          ()
          {
            retc = (fun v -> v);
            exnc = raise;
            effc =
              (fun (type a) (e : a Effect.t) ->
                match e with
                | NameError (_, _, message) ->
                    failwithf "Name error propagated: %s" message ()
                | _ -> None);
          };
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

let main files mode =
  List.iter files ~f:(fun fname ->
      Eval.push_mod (String.capitalize fname)
        (`Mod (Mod.with_file (module Eval) fname)));

  match mode with
  | `Repl ->
      Out_channel.(flush stdout);
      repl "" (Mod.make_new (module Eval) (Some "Repl")) ()
  | `Run -> ()

let command =
  Command.basic ~summary:"Run the mlisp interpreter"
    (let%map_open.Command mode = mode_param and file_names = files_param in
     fun () ->
       match mode with
       | Some "repl" -> main file_names `Repl
       | _ -> main file_names `Run)

let _ = Command_unix.run command
