(lang dune 3.16)

(name utilities)

(generate_opam_files true)

(source
 (github jcarlson23/git@github.com:jcarlson23/ocaml-utils.git))

(authors "Jared Carlson")

(maintainers "Jared Carlson")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name utilities)
 (synopsis "This is a library of various utilities, largely meant to be around parsing the AST")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
