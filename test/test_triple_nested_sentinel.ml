open! Float_u

(* Three levels of sentinel-backed options using float NaN throughout.
   In sentinel mode Option.t = t, so is_none at each level delegates to
   the inner contract's is_none. *)

module Inner = struct
  type t = float# [@@deriving unboxed_option { sentinel = true }]
end

module Middle = struct
  type t =
    #{ a : Inner.t
     ; b : float#
     }
  [@@deriving unboxed_option { sentinel = true }]
end

module Outer = struct
  type t =
    #{ m : Middle.t
     ; c : float#
     }
  [@@deriving unboxed_option { sentinel = true }]
end

let () =
  assert (Inner.Option.is_none Inner.Option.none);
  assert (Middle.Option.is_none Middle.Option.none);
  assert (Outer.Option.is_none Outer.Option.none);
  (* In sentinel mode Option.t = t, so none is the raw NaN value *)
  assert (Float_u.is_nan Inner.Option.none);
  let mn = Middle.Option.none in
  assert (Float_u.is_nan mn.#a);
  assert (Float_u.is_nan mn.#b);
  let on = Outer.Option.none in
  let on_m = on.#m in
  assert (Float_u.is_nan on_m.#a);
  assert (Float_u.is_nan on_m.#b);
  assert (Float_u.is_nan on.#c);
  (* Changing any field at any depth makes it some *)
  let nan = Float_u.nan () in
  assert (not (Inner.Option.is_none #1.0));
  assert (not (Middle.Option.is_none #{ Middle.a = #1.0; b = nan }));
  assert (not (Middle.Option.is_none #{ Middle.a = nan; b = #1.0 }));
  assert (not (Outer.Option.is_none #{ Outer.m = Middle.Option.none; c = #1.0 }));
  assert (not (Outer.Option.is_none #{ Outer.m = #{ Middle.a = #1.0; b = nan }; c = nan }));
  assert (not (Outer.Option.is_none #{ Outer.m = #{ Middle.a = nan; b = #1.0 }; c = nan }));
  (* some/value_exn round-trip *)
  let v = Outer.Option.some #{ Outer.m = #{ Middle.a = #1.5; b = #2.5 }; c = #3.5 } in
  assert (Outer.Option.is_some v);
  assert (not (Outer.Option.is_none v));
  let vm = v.#m in
  assert (Float_u.equal vm.#a #1.5);
  assert (Float_u.equal vm.#b #2.5);
  assert (Float_u.equal v.#c #3.5);
  (* value ~default *)
  let default : Outer.t = #{ Outer.m = #{ Middle.a = #0.5; b = #0.5 }; c = #0.0 } in
  let resolved = Outer.Option.value Outer.Option.none ~default in
  let resolved_m = resolved.#m in
  assert (Float_u.equal resolved_m.#a #0.5);
  assert (Float_u.equal resolved.#c #0.0);
  let resolved2 = Outer.Option.value v ~default in
  assert (Float_u.equal resolved2.#c #3.5)
;;
