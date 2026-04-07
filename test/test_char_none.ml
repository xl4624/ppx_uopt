open Test_helpers

module Char_token = struct
  type t = char# [@@deriving unboxed_option { none = #'\000' }]
end

let () =
  assert (Char_token.Option.is_none Char_token.Option.none);
  let v = Char_token.Option.some #'x' in
  assert (Char_token.Option.is_some v);
  assert (eq_char_u (Char_token.Option.value_exn v) #'x')
