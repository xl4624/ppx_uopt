type scalar_value = float# [@@deriving unboxed_option]

module Scalar_value_option = Option

let () =
  let raised =
    try
      let _ = Scalar_value_option.value_exn Scalar_value_option.none in
      false
    with
    | Failure _ -> true
  in
  assert raised
;;
