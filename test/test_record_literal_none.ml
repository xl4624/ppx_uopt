open Test_helpers

type packed_point =
  #{ x : int8#
   ; y : int32#
   }
[@@deriving unboxed_option { none = #{ x = #0s; y = #0l } }]

module Packed_point_option = Option

let () =
  assert (Packed_point_option.is_none Packed_point_option.none);
  assert (Packed_point_option.is_none #{ x = #0s; y = #0l });
  assert (not (Packed_point_option.is_none #{ x = #0s; y = #7l }));
  assert (not (Packed_point_option.is_none #{ x = #7s; y = #0l }));
  let p = Packed_point_option.some #{ x = #7s; y = #9l } in
  assert (Packed_point_option.is_some p);
  let v = Packed_point_option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Int32_u.equal v.#y #9l)
;;
