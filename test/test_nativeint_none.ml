module Nativeint_token = struct
  type t = nativeint# [@@deriving unboxed_option { none = #0n }]
end

let () =
  assert (Nativeint_token.Option.is_none Nativeint_token.Option.none);
  let v = Nativeint_token.Option.some #7n in
  assert (Nativeint_token.Option.is_some v);
  assert (Nativeint_u.equal (Nativeint_token.Option.value_exn v) #7n)
;;
