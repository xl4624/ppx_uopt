(lang dune 3.22)

(name ppx_uopt)

(source
 (github xl4624/ppx_uopt))

(authors "Xiaomin Liu <xl4624@nyu.edu>")

(license LICENSE)

(package
 (name ppx_uopt)
 (allow_empty)
 (synopsis "[@@deriving unboxed_option]")
 (description "PPX deriver for allocation-free Option helpers on unboxed types")
 (depends
  ocaml
  dune
  ppxlib
  ppxlib_jane)
 (tags
  ("ppx" "oxcaml" "unboxed" "option" "deriving")))
