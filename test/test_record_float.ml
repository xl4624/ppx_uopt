open! Float_u

type pair =
  #{ x : Float_u.t
   ; y : Float_u.t
   }
[@@deriving unboxed_option]

let () =
  assert (Option.is_none Option.none);
  let none_payload = Option.unchecked_value Option.none in
  assert (Float_u.is_nan none_payload.#x);
  assert (Float_u.is_nan none_payload.#y);
  let p = Option.some #{ x = #1.0; y = #2.0 } in
  assert (Option.is_some p);
  assert (not (Option.is_none p));
  let v = Option.value_exn p in
  assert (Float_u.equal v.#x #1.0);
  assert (Float_u.equal v.#y #2.0)
;;
