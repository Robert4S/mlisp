# Mlisp

Mlisp is a basic lisp grammar and interpreter, including a fragile repl that can either work on its own, or load a Mlisp file and call functions from there.

The grammar can be found at Parser.mly, but not all of it has been implemented.

The basic command line arguments so far are:

<mode> <filename>

mode: 
-r loads the file at <filename>    and enters the repl. -c simply runs the file and prints the evaluation of the last form in the file. an empty argument list enters the repl without any context.

The code is riddled with exceptions and todos, so it will break easily.
