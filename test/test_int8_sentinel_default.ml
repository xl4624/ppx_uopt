module Int8_opt = struct
  type t = int8# [@@deriving unboxed_option { sentinel = true }]
end

external eq_int8_u : int8# -> int8# -> bool = "%int8#_equal"

let () =
  assert (Int8_opt.Option.is_none Int8_opt.Option.none);
  let v = Int8_opt.Option.some #7s in
  assert (Int8_opt.Option.is_some v);
  assert (eq_int8_u (Int8_opt.Option.value_exn v) #7s);
  assert (not (Int8_opt.Option.is_none (Int8_opt.Option.some #0s)));
  assert (not (Int8_opt.Option.is_none (Int8_opt.Option.some #127s)))
;;
