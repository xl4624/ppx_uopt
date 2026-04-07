open Test_helpers

module Int_token = struct
  type t = int# [@@deriving unboxed_option { none = 0 }]
end

let () =
  assert (Int_token.Option.is_none Int_token.Option.none);
  let v = Int_token.Option.some (of_int_u 7) in
  assert (Int_token.Option.is_some v);
  assert (eq_int_u (Int_token.Option.value_exn v) (of_int_u 7))
;;
