open! Float_u

type pair =
  #{ x : Float_u.t
   ; y : Float_u.t
   }
[@@deriving unboxed_option]

let () =
  assert (Option.is_none Option.none);
  assert (Option.is_none #{ x = Float_u.nan (); y = #1.0 });
  assert (Option.is_none #{ x = #1.0; y = Float_u.nan () });
  let p = Option.some #{ x = #1.0; y = #2.0 } in
  assert (Option.is_some p);
  assert (not (Option.is_none p));
  let v = Option.value_exn p in
  assert (Float_u.equal v.#x #1.0);
  assert (Float_u.equal v.#y #2.0)
;;
