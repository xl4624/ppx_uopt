open! Float_u

module Int64_token = struct
  type t = int64# [@@deriving unboxed_option { none = #0L }]
end

let () =
  let none = Int64_token.Option.none in
  assert (Int64_token.Option.is_none none);
  let v = Int64_token.Option.some #7L in
  assert (Int64_token.Option.is_some v);
  assert (not (Int64_token.Option.is_none v));
  assert (Int64_u.equal (Int64_token.Option.value none ~default:#9L) #9L);
  assert (Int64_u.equal (Int64_token.Option.value_exn v) #7L)
;;
