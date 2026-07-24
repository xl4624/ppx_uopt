open Test_helpers

module Pair = struct
  type t = #(int8# * char#)
  [@@deriving unboxed_option]
end

let () =
  let n = Pair.Option.none in
  assert (Pair.Option.is_none n);
  assert (not (Pair.Option.is_some n));
  let s = Pair.Option.some #(#5s, #'x') in
  assert (Pair.Option.is_some s);
  assert (not (Pair.Option.is_none s));
  let #(a, b) = Pair.Option.value_exn s in
  assert (eq_int8_u a #5s);
  assert (eq_char_u b #'x')
;;
