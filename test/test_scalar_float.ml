open! Float_u

type scalar_value = float# [@@deriving unboxed_option]

module Scalar_value_option = Option

let () =
  let none = Scalar_value_option.none in
  assert (Scalar_value_option.is_none none);
  assert (not (Scalar_value_option.is_some none));
  assert (Float_u.is_nan (Scalar_value_option.unchecked_value none));
  let v = Scalar_value_option.some #3.14 in
  assert (Scalar_value_option.is_some v);
  assert (not (Scalar_value_option.is_none v));
  let default = #0.0 in
  let r = Scalar_value_option.value none ~default in
  assert (Float_u.equal r default);
  let r2 = Scalar_value_option.value v ~default in
  assert (Float_u.equal r2 #3.14)
;;
