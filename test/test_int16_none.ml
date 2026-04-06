open Test_helpers

type int16_token = int16# [@@deriving unboxed_option { none = #0S }]

module Int16_token_option = Option

let () =
  assert (Int16_token_option.is_none Int16_token_option.none);
  let v = Int16_token_option.some #7S in
  assert (Int16_token_option.is_some v);
  assert (eq_int16_u (Int16_token_option.value_exn v) #7S)
;;
