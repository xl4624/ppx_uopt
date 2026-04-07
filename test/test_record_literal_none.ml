open Test_helpers

module Packed_point = struct
  type t =
    #{ x : int8#
     ; y : int32#
     }
  [@@deriving unboxed_option { none = #{ x = #0s; y = #0l } }]
end

let () =
  assert (Packed_point.Option.is_none Packed_point.Option.none);
  assert (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = #0l });
  assert (not (Packed_point.Option.is_none #{ Packed_point.x = #0s; y = #7l }));
  assert (not (Packed_point.Option.is_none #{ Packed_point.x = #7s; y = #0l }));
  let p = Packed_point.Option.some #{ Packed_point.x = #7s; y = #9l } in
  assert (Packed_point.Option.is_some p);
  let v = Packed_point.Option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Int32_u.equal v.#y #9l)
;;
