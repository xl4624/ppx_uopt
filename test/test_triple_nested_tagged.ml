open! Float_u

(* Three levels of tagged option nesting:
   Outer.t = #{ m: Middle.t; c: float# }
   Middle.t = #{ a: Inner.t; b: float# }
   Inner.t  = float# *)

module Inner = struct
  type t = float# [@@deriving unboxed_option]
end

module Middle = struct
  type t =
    #{ a : Inner.t
     ; b : float#
     }
  [@@deriving unboxed_option]
end

module Outer = struct
  type t =
    #{ m : Middle.t
     ; c : float#
     }
  [@@deriving unboxed_option]
end

let () =
  assert (Inner.Option.is_none Inner.Option.none);
  assert (Middle.Option.is_none Middle.Option.none);
  assert (Outer.Option.is_none Outer.Option.none);

  (* NaN propagates through the default payload at every level *)
  let outer_payload = Outer.Option.unchecked_value Outer.Option.none in
  let m = outer_payload.#m in
  assert (Float_u.is_nan m.#a);
  assert (Float_u.is_nan m.#b);
  assert (Float_u.is_nan outer_payload.#c);

  let mid_payload = Middle.Option.unchecked_value Middle.Option.none in
  assert (Float_u.is_nan mid_payload.#a);
  assert (Float_u.is_nan mid_payload.#b);

  (* some/value_exn round-trip through all three levels *)
  let mid : Middle.t = #{ Middle.a = #1.5; b = #2.5 } in
  let outer_val = Outer.Option.some #{ Outer.m = mid; c = #3.5 } in
  assert (Outer.Option.is_some outer_val);
  assert (not (Outer.Option.is_none outer_val));
  let ov = Outer.Option.value_exn outer_val in
  let ov_m = ov.#m in
  assert (Float_u.equal ov_m.#a #1.5);
  assert (Float_u.equal ov_m.#b #2.5);
  assert (Float_u.equal ov.#c #3.5);

  (* value ~default falls back correctly at outer level *)
  let default : Outer.t = #{ Outer.m = #{ Middle.a = #0.5; b = #0.5 }; c = #0.0 } in
  let resolved = Outer.Option.value Outer.Option.none ~default in
  let resolved_m = resolved.#m in
  assert (Float_u.equal resolved_m.#a #0.5);
  assert (Float_u.equal resolved_m.#b #0.5);
  assert (Float_u.equal resolved.#c #0.0);

  (* value ~default returns the some value unchanged *)
  let resolved2 = Outer.Option.value outer_val ~default in
  let resolved2_m = resolved2.#m in
  assert (Float_u.equal resolved2_m.#a #1.5);
  assert (Float_u.equal resolved2.#c #3.5);

  (* Middle option works independently *)
  let mid_opt = Middle.Option.some #{ Middle.a = #7.0; b = #8.0 } in
  assert (Middle.Option.is_some mid_opt);
  let mid_val = Middle.Option.value_exn mid_opt in
  assert (Float_u.equal mid_val.#a #7.0);
  assert (Float_u.equal mid_val.#b #8.0)
