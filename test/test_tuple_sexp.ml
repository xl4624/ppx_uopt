module Pair = struct
  type t = #(int8# * int32#) [@@deriving unboxed_option { none = #(#0s, #0l) }]
end

let sexp_to_string sexp = Sexplib0.Sexp.to_string sexp

let () =
  let none_sexp = Pair.Option.sexp_of_t Pair.Option.none in
  assert (String.equal (sexp_to_string none_sexp) "()");
  let v = Pair.Option.some #(#3s, #7l) in
  let some_sexp = Pair.Option.sexp_of_t v in
  assert (String.equal (sexp_to_string some_sexp) "((3 7))")
;;
