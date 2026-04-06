type scalar_value = float# [@@deriving unboxed_option]

module Scalar_value_option = Option

let _test_optional_u (x : Scalar_value_option.t) =
  match%optional_u (x : Scalar_value_option.t) with
  | None -> #0.0
  | Some v -> v
;;
