open! Float_u

type scalar_value = float# [@@deriving unboxed_option { sentinel = true }]

module Scalar_value_option = Option

let () =
  assert (Scalar_value_option.is_none Scalar_value_option.none);
  assert (Float_u.is_nan Scalar_value_option.none);
  let v = Scalar_value_option.some #3.5 in
  assert (Scalar_value_option.is_some v);
  assert (Float_u.equal (Scalar_value_option.value_exn v) #3.5)
;;
