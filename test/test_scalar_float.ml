open! Float_u

module Scalar_value = struct
  type t = float# [@@deriving unboxed_option]
end

let () =
  let none = Scalar_value.Option.none in
  assert (Scalar_value.Option.is_none none);
  assert (not (Scalar_value.Option.is_some none));
  assert (Float_u.is_nan (Scalar_value.Option.unchecked_value none));
  let v = Scalar_value.Option.some #3.14 in
  assert (Scalar_value.Option.is_some v);
  assert (not (Scalar_value.Option.is_none v));
  let default = #0.0 in
  let r = Scalar_value.Option.value none ~default in
  assert (Float_u.equal r default);
  let r2 = Scalar_value.Option.value v ~default in
  assert (Float_u.equal r2 #3.14)
;;
