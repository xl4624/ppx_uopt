type float32_token = float32# [@@deriving unboxed_option]

module Float32_token_option = Option

let () =
  assert (Float32_token_option.is_none Float32_token_option.none);
  let v = Float32_token_option.some #1.25s in
  assert (Float32_token_option.is_some v);
  assert (Float32_u.equal (Float32_token_option.value_exn v) #1.25s)
;;
