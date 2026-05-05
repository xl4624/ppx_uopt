open Test_helpers
open! Float_u

(* Partial-override semantics: [is_none] checks only the fields listed in
   [none = #{ ... }]. Omitted fields are payload-only and freely take any value, even
   values matching the synthesised default for that field's type. To make [is_none] check
   all fields, list them all explicitly. *)

module Packed_point = struct
  type t =
    #{ x : int8#
     ; y : float#
     }
  [@@deriving unboxed_option { none = #{ x = #0s } }]
end

let () =
  assert (Packed_point.Option.is_none Packed_point.Option.none);
  (* Synthesised [none] still has a default for [y]; the partial-override comment in the
     README explains [Float_u.nan ()] for omitted float fields. *)
  assert (eq_int8_u Packed_point.Option.none.#x #0s);
  assert (Float_u.is_nan Packed_point.Option.none.#y);
  (* Only [x] discriminates: y can be anything when x = #0s. *)
  assert (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = Float_u.nan });
  assert (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = #7.0 });
  (* Differing in x makes the value some regardless of y. *)
  assert (not (Packed_point.Option.is_none #{ Packed_point.x = #1s; y = Float_u.nan }));
  let p_same_x = Packed_point.Option.some #{ Packed_point.x = #0s; y = #7.0 } in
  assert (Packed_point.Option.is_none p_same_x);
  let p = Packed_point.Option.some #{ Packed_point.x = #7s; y = #9.0 } in
  assert (Packed_point.Option.is_some p);
  let v = Packed_point.Option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Float_u.equal v.#y #9.0)
;;
