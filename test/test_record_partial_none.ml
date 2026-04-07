open Test_helpers
open! Float_u

module Packed_point = struct
  type t =
    #{ x : int8#
     ; y : float#
     }
  [@@deriving unboxed_option { none = #{ x = #0s } }]
end

let () =
  assert (Packed_point.Option.is_none Packed_point.Option.none);
  assert (eq_int8_u Packed_point.Option.none.#x #0s);
  assert (Float_u.is_nan Packed_point.Option.none.#y);
  assert (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = Float_u.nan () });
  assert (not (Packed_point.Option.is_none #{ Packed_point.x = #1s; y = Float_u.nan () }));
  assert (not (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = #7.0 }));
  let p_same_x = Packed_point.Option.some #{ Packed_point.x = #0s; y = #7.0 } in
  assert (Packed_point.Option.is_some p_same_x);
  let v_same_x = Packed_point.Option.value_exn p_same_x in
  assert (eq_int8_u v_same_x.#x #0s);
  assert (Float_u.equal v_same_x.#y #7.0);
  let p = Packed_point.Option.some #{ Packed_point.x = #7s; y = #9.0 } in
  assert (Packed_point.Option.is_some p);
  let v = Packed_point.Option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Float_u.equal v.#y #9.0)
;;
