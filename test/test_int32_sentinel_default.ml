module Int32_opt = struct
  type t = int32# [@@deriving unboxed_option { sentinel = true }]
end

let () =
  assert (Int32_opt.Option.is_none Int32_opt.Option.none);
  let v = Int32_opt.Option.some #7l in
  assert (Int32_opt.Option.is_some v);
  assert (Int32_u.equal (Int32_opt.Option.value_exn v) #7l);
  assert (not (Int32_opt.Option.is_none (Int32_opt.Option.some #0l)));
  assert (not (Int32_opt.Option.is_none (Int32_opt.Option.some #2147483647l)))
;;
