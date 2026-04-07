open Test_helpers

module Int8_token = struct
  type t = int8# [@@deriving unboxed_option { none = #2s }]
end

let () =
  assert (Int8_token.Option.is_none Int8_token.Option.none);
  let v = Int8_token.Option.some #7s in
  assert (Int8_token.Option.is_some v);
  assert (eq_int8_u (Int8_token.Option.value_exn v) #7s)
;;
