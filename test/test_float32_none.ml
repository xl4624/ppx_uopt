module Float32_token = struct
  type t = float32# [@@deriving unboxed_option]
end

let () =
  assert (Float32_token.Option.is_none Float32_token.Option.none);
  let v = Float32_token.Option.some #1.25s in
  assert (Float32_token.Option.is_some v);
  assert (Float32_u.equal (Float32_token.Option.value_exn v) #1.25s)
;;
