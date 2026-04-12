open! Float_u

module Inner = struct
  type t = float# [@@deriving unboxed_option { sentinel = true }]
end

module Outer = struct
  type t =
    #{ a : Inner.t
     ; b : float#
     }
  [@@deriving unboxed_option]
end

let sexp_to_string sexp = Sexplib0.Sexp.to_string sexp

let () =
  let v = Outer.Option.some #{ Outer.a = #1.5; b = #2.5 } in
  assert (String.equal (sexp_to_string (Outer.Option.sexp_of_t v)) "(((a 1.5)(b 2.5)))");
  assert (String.equal (sexp_to_string (Outer.Option.sexp_of_t Outer.Option.none)) "()")
;;
