open! Float_u

module Pair = struct
  type t =
    #{ x : Float_u.t
     ; y : Float_u.t
     }
  [@@deriving unboxed_option]
end

let () =
  assert (Pair.Option.is_none Pair.Option.none);
  let none_payload = Pair.Option.unchecked_value Pair.Option.none in
  assert (Float_u.is_nan none_payload.#x);
  assert (Float_u.is_nan none_payload.#y);
  let p = Pair.Option.some #{ Pair.x = #1.0; y = #2.0 } in
  assert (Pair.Option.is_some p);
  assert (not (Pair.Option.is_none p));
  let v = Pair.Option.value_exn p in
  assert (Float_u.equal v.#x #1.0);
  assert (Float_u.equal v.#y #2.0)
;;
