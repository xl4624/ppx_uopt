open Test_helpers

type int8_token = int8# [@@deriving unboxed_option { none = #0s }]

module Int8_token_option = Option

let () =
  assert (Int8_token_option.is_none Int8_token_option.none);
  let v = Int8_token_option.some #7s in
  assert (Int8_token_option.is_some v);
  assert (eq_int8_u (Int8_token_option.value_exn v) #7s)
;;
