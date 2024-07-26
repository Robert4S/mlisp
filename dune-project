(lang dune 3.16)

(name mlisp)

(generate_opam_files true)
(source
 (github Robert4S/mlisp))

(authors "Robert4S")

(maintainers "Robert4S")

(license MIT)

(using menhir 3.0)

(package
 (name mlisp)
 (synopsis "Lisp interpreter")
 (description "Simple lisp grammar, and reader written in ocampl")
 (depends ocaml dune)
 (tags
  ("lisp" "parser" "interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
