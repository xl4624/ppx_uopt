open Test_helpers

type char_token = char# [@@deriving unboxed_option { none = #'\000' }]

module Char_token_option = Option

let () =
  assert (Char_token_option.is_none Char_token_option.none);
  let v = Char_token_option.some #'x' in
  assert (Char_token_option.is_some v);
  assert (eq_char_u (Char_token_option.value_exn v) #'x')
