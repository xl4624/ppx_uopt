open! Float_u

module Scalar_value = struct
  type t = float# [@@deriving unboxed_option { sentinel = true }]
end

let () =
  assert (Scalar_value.Option.is_none Scalar_value.Option.none);
  assert (Float_u.is_nan Scalar_value.Option.none);
  let v = Scalar_value.Option.some #3.5 in
  assert (Scalar_value.Option.is_some v);
  assert (Float_u.equal (Scalar_value.Option.value_exn v) #3.5)
;;
