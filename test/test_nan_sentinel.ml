open! Float_u

(* NaN-as-sentinel for float# / float32# / float. The deriver detects [Float_u.nan] /
   [Float32_u.nan] / [Float.nan] in [none = ...] and emits [is_nan]-based [is_none]
   instead of [equal]-based, since IEEE 754 makes NaN unequal to itself. *)

module Float_u_token = struct
  type t = float# [@@deriving unboxed_option { none = Float_u.nan }]
end

module Float32_u_token = struct
  type t = float32# [@@deriving unboxed_option { none = Float32_u.nan }]
end

module Mixed = struct
  type t =
    #{ x : float
     ; y : int
     }
  [@@deriving unboxed_option { none = #{ x = Float.nan; y = -1 } }]
end

let () =
  (* float#: Float_u.nan as sentinel *)
  assert (Float_u_token.Option.is_none Float_u_token.Option.none);
  assert (Float_u.is_nan Float_u_token.Option.none);
  assert (Float_u_token.Option.is_none (Float_u_token.Option.some Float_u.nan));
  let v = Float_u_token.Option.some #3.5 in
  assert (Float_u_token.Option.is_some v);
  assert (Float_u.equal (Float_u_token.Option.value_exn v) #3.5);
  (* float32# *)
  assert (Float32_u_token.Option.is_none Float32_u_token.Option.none);
  assert (Float32_u_token.Option.is_none (Float32_u_token.Option.some Float32_u.nan));
  let v = Float32_u_token.Option.some #1.25s in
  assert (Float32_u_token.Option.is_some v);
  (* boxed float field with NaN sentinel *)
  assert (Mixed.Option.is_none Mixed.Option.none);
  assert (Mixed.Option.is_none #{ Mixed.x = Float.nan; y = -1 });
  assert (Mixed.Option.is_none #{ Mixed.x = Stdlib.nan; y = -1 });
  (* Differing y or non-NaN x makes it some. *)
  assert (not (Mixed.Option.is_none #{ Mixed.x = Float.nan; y = 0 }));
  assert (not (Mixed.Option.is_none #{ Mixed.x = 1.0; y = -1 }));
  let v = Mixed.Option.some #{ Mixed.x = 2.5; y = 7 } in
  assert (Mixed.Option.is_some v);
  let payload = Mixed.Option.value_exn v in
  assert (Float.equal payload.#x 2.5);
  assert (Stdlib.( = ) payload.#y 7)
;;
