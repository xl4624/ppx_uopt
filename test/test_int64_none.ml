open! Float_u

type int64_token = int64# [@@deriving unboxed_option { none = #0L }]

module Int64_token_option = Option

let () =
  let none = Int64_token_option.none in
  assert (Int64_token_option.is_none none);
  let v = Int64_token_option.some #7L in
  assert (Int64_token_option.is_some v);
  assert (not (Int64_token_option.is_none v));
  assert (Int64_u.equal (Int64_token_option.value none ~default:#9L) #9L);
  assert (Int64_u.equal (Int64_token_option.value_exn v) #7L)
;;
