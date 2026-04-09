open Test_helpers
open! Float_u

(* Five-field record with a partial none override.
   int8/int32/int64 use explicit sentinels; float fields fall back to NaN. *)

module Wide = struct
  type t =
    #{ a : int8#
     ; b : int32#
     ; c : int64#
     ; d : float#
     ; e : float#
     }
  [@@deriving unboxed_option { none = #{ a = #0s; b = #0l; c = #0L } }]
end

let () =
  assert (Wide.Option.is_none Wide.Option.none);
  let n = Wide.Option.none in
  assert (eq_int8_u n.#a #0s);
  assert (Int32_u.equal n.#b #0l);
  assert (Int64_u.equal n.#c #0L);
  assert (Float_u.is_nan n.#d);
  assert (Float_u.is_nan n.#e);
  (* Every field independently gates is_none *)
  let nan = Float_u.nan () in
  assert (not (Wide.Option.is_none #{ Wide.a = #1s; b = #0l; c = #0L; d = nan; e = nan }));
  assert (not (Wide.Option.is_none #{ Wide.a = #0s; b = #1l; c = #0L; d = nan; e = nan }));
  assert (not (Wide.Option.is_none #{ Wide.a = #0s; b = #0l; c = #1L; d = nan; e = nan }));
  assert (not (Wide.Option.is_none #{ Wide.a = #0s; b = #0l; c = #0L; d = #1.0; e = nan }));
  assert (not (Wide.Option.is_none #{ Wide.a = #0s; b = #0l; c = #0L; d = nan; e = #1.0 }));
  (* Exact sentinel matches none *)
  assert (Wide.Option.is_none #{ Wide.a = #0s; b = #0l; c = #0L; d = nan; e = nan });
  (* some/value_exn round-trip *)
  let v = Wide.Option.some #{ Wide.a = #7s; b = #8l; c = #9L; d = #1.5; e = #2.5 } in
  assert (Wide.Option.is_some v);
  let r = Wide.Option.value_exn v in
  assert (eq_int8_u r.#a #7s);
  assert (Int32_u.equal r.#b #8l);
  assert (Int64_u.equal r.#c #9L);
  assert (Float_u.equal r.#d #1.5);
  assert (Float_u.equal r.#e #2.5);
  (* value ~default *)
  let default : Wide.t = #{ Wide.a = #1s; b = #2l; c = #3L; d = #0.5; e = #0.5 } in
  let resolved = Wide.Option.value Wide.Option.none ~default in
  assert (eq_int8_u resolved.#a #1s);
  assert (Int32_u.equal resolved.#b #2l);
  assert (Float_u.equal resolved.#d #0.5);
  let resolved2 = Wide.Option.value v ~default in
  assert (eq_int8_u resolved2.#a #7s);
  assert (Float_u.equal resolved2.#d #1.5)
;;
