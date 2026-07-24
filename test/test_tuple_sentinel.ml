open Test_helpers

module Triple = struct
  type t = #(int8# * int32# * int64#)
  [@@deriving unboxed_option { none = #(#12s, #0l, #0L) }]
end

let () =
  assert (Triple.Option.is_none Triple.Option.none);
  assert (Triple.Option.is_none #(#12s, #0l, #0L));
  (* every component must match the sentinel - partial matches are still [Some] *)
  assert (not (Triple.Option.is_none #(#12s, #7l, #0L)));
  assert (not (Triple.Option.is_none #(#3s, #0l, #0L)));
  let v = Triple.Option.some #(#3s, #7l, #9L) in
  assert (Triple.Option.is_some v);
  let #(a, b, c) = Triple.Option.value_exn v in
  assert (eq_int8_u a #3s);
  assert (Int32_u.equal b #7l);
  assert (Int64_u.equal c #9L)
;;
