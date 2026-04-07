module Int32_token = struct
  type t = int32# [@@deriving unboxed_option { none = #0l }]
end

let () =
  assert (Int32_token.Option.is_none Int32_token.Option.none);
  let v = Int32_token.Option.some #7l in
  assert (Int32_token.Option.is_some v);
  assert (Int32_u.equal (Int32_token.Option.value_exn v) #7l)
;;
