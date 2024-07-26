%{
open Ast
%}

%token <int> INT
%token <string> ATOM
%token <float> FLOAT
%token <string> STRING

%token DEF
%token DEFUN
%token LPAREN "("
%token RPAREN ")"
%token EOF
%token QUOTE "'"
%token DOT "."

%start <expr option> prog
%%

prog:
  | EOF { None }
  | es = list(expr); EOF { Some (List es) }
;

expr:
  | car = expr; DOT; cdr = expr {ConsCell (car, cdr)}
  | QUOTE; e = expr {Quoted e}
  | s = STRING {String s}
  | a = ATOM {Atom a}
  | i = INT {Int i}
  | f = FLOAT {Float f}
  | LPAREN; DEF; name = ATOM; e = expr; RPAREN {Def (name, e)}
  | LPAREN; DEFUN; name = ATOM; LPAREN; args = list(ATOM); RPAREN; e = expr; RPAREN {Defun (name, args, e)}
  | LPAREN; es = list(expr); RPAREN {List es}
;
