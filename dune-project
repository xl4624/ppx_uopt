(lang dune 3.22)

(name ppx_uopt)

(generate_opam_files true)

(source
 (github xl4624/ppx_uopt))

(authors "Xiaomin Liu <xl4624@nyu.edu>")

(maintainers "Xiaomin Liu <xl4624@nyu.edu>")

(license MIT)

(package
 (name ppx_uopt)
 (synopsis "[@@deriving unboxed_option]")
 (description "PPX deriver for allocation-free Option helpers on unboxed types")
 (depends
  (ocaml (>= 5.0))
  (ppxlib (>= 0.32))
  ppxlib_jane
  (unboxed :with-test)
  (ppx_jane :with-test))
 (tags
  ("ppx" "oxcaml" "unboxed" "option" "deriving")))
