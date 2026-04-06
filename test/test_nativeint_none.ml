type nativeint_token = nativeint# [@@deriving unboxed_option { none = #0n }]

module Nativeint_token_option = Option

let () =
  assert (Nativeint_token_option.is_none Nativeint_token_option.none);
  let v = Nativeint_token_option.some #7n in
  assert (Nativeint_token_option.is_some v);
  assert (Nativeint_u.equal (Nativeint_token_option.value_exn v) #7n)
;;
