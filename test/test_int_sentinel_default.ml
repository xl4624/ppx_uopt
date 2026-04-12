open Test_helpers

module Int_opt = struct
  type t = int# [@@deriving unboxed_option { sentinel = true }]
end

let () =
  assert (Int_opt.Option.is_none Int_opt.Option.none);
  let v = Int_opt.Option.some (of_int_u 7) in
  assert (Int_opt.Option.is_some v);
  assert (eq_int_u (Int_opt.Option.value_exn v) (of_int_u 7));
  assert (not (Int_opt.Option.is_none (Int_opt.Option.some (of_int_u 0))));
  assert (not (Int_opt.Option.is_none (Int_opt.Option.some (of_int_u max_int))))
;;
