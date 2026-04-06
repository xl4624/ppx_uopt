open Test_helpers

type int_token = int# [@@deriving unboxed_option { none = 0 }]

module Int_token_option = Option

let () =
  assert (Int_token_option.is_none Int_token_option.none);
  let v = Int_token_option.some (of_int_u 7) in
  assert (Int_token_option.is_some v);
  assert (eq_int_u (Int_token_option.value_exn v) (of_int_u 7))
;;
