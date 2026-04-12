open! Float_u

module Pair = struct
  type t =
    #{ x : Float_u.t
     ; y : Float_u.t
     }
  [@@deriving unboxed_option]
end

let sexp_to_string sexp = Sexplib0.Sexp.to_string sexp

let () =
  (* Record none *)
  let none_sexp = Pair.Option.sexp_of_t Pair.Option.none in
  assert (String.equal (sexp_to_string none_sexp) "()");
  (* Record some *)
  let v = Pair.Option.some #{ Pair.x = #1.5; y = #2.5 } in
  let some_sexp = Pair.Option.sexp_of_t v in
  assert (String.equal (sexp_to_string some_sexp) "(((x 1.5)(y 2.5)))")
;;
