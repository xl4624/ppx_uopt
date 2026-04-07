module Scalar_value = struct
  type t = float# [@@deriving unboxed_option]
end

let _test_optional_u (x : Scalar_value.Option.t) =
  match%optional_u.Scalar_value.Option x with
  | None -> #0.0
  | Some v -> v
;;
