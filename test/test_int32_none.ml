type int32_token = int32# [@@deriving unboxed_option { none = #0l }]

module Int32_token_option = Option

let () =
  assert (Int32_token_option.is_none Int32_token_option.none);
  let v = Int32_token_option.some #7l in
  assert (Int32_token_option.is_some v);
  assert (Int32_u.equal (Int32_token_option.value_exn v) #7l)
;;
