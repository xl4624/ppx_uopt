module Scalar_value = struct
  type t = float# [@@deriving unboxed_option]
end

let () =
  let raised =
    try
      let _ = Scalar_value.Option.value_exn Scalar_value.Option.none in
      false
    with
    | Failure _ -> true
  in
  assert raised
;;
