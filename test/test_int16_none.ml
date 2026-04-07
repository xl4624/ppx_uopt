open Test_helpers

module Int16_token = struct
  type t = int16# [@@deriving unboxed_option { none = #0S }]
end

let () =
  assert (Int16_token.Option.is_none Int16_token.Option.none);
  let v = Int16_token.Option.some #7S in
  assert (Int16_token.Option.is_some v);
  assert (eq_int16_u (Int16_token.Option.value_exn v) #7S)
;;
