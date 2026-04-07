open Test_helpers
open! Float_u

type packed_point =
  #{ x : int8#
   ; y : float#
   }
[@@deriving unboxed_option { none = #{ x = #0s } }]

module Packed_point_option = Option

let () =
  assert (Packed_point_option.is_none Packed_point_option.none);
  assert (eq_int8_u Packed_point_option.none.#x #0s);
  assert (Float_u.is_nan Packed_point_option.none.#y);
  assert (Packed_point_option.is_none #{ x = #0s; y = Float_u.nan () });
  assert (not (Packed_point_option.is_none #{ x = #1s; y = Float_u.nan () }));
  assert (not (Packed_point_option.is_none #{ x = #0s; y = #7.0 }));
  let p = Packed_point_option.some #{ x = #7s; y = #9.0 } in
  assert (Packed_point_option.is_some p);
  let v = Packed_point_option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Float_u.equal v.#y #9.0)
;;
