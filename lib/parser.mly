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
%token FN
%token LBRAC "{"
%token RBRAC "}"

%start <expr option> prog
%%

prog:
  | EOF { None }
  | es = list(expr); EOF { Some (List es) }
;

expr:
  | LPAREN; car = expr; DOT; cdr = expr; RPAREN {ConsCell (car, cdr)}
  | QUOTE; e = expr {Quoted e}
  | s = STRING {String s}
  | a = ATOM {Atom a}
  | i = INT {Int i}
  | f = FLOAT {Float f}
  | LPAREN; FN; LPAREN; args = list(ATOM); RPAREN; e = expr; RPAREN; {Fn (args, e)}
  | LPAREN; DEF; name = ATOM; e = expr; RPAREN {Def (name, e)}
  | LPAREN; DEFUN; name = ATOM; LPAREN; args = list(ATOM); RPAREN; e = expr; RPAREN {Defun (name, args, e)}
  | LBRAC; es = list(expr); RBRAC {Map es}
  | LPAREN; es = list(expr); RPAREN {List es}
;
