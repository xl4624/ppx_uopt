module Int64_opt = struct
  type t = int64# [@@deriving unboxed_option { sentinel = true }]
end

let () =
  assert (Int64_opt.Option.is_none Int64_opt.Option.none);
  let v = Int64_opt.Option.some #7L in
  assert (Int64_opt.Option.is_some v);
  assert (Int64_u.equal (Int64_opt.Option.value_exn v) #7L);
  assert (not (Int64_opt.Option.is_none (Int64_opt.Option.some #0L)))
;;
