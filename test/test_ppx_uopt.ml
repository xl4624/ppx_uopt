open! Float_u

external eq_int8_u : int8# -> int8# -> bool = "%int8#_equal"
external eq_int16_u : int16# -> int16# -> bool = "%int16#_equal"
external of_int_u : int -> int# = "%int#_of_int"
external eq_int_u : int# -> int# -> bool = "%int#_equal"
external char_to_int8_u : char# -> int8# = "%identity"

let eq_char_u x y = eq_int8_u (char_to_int8_u x) (char_to_int8_u y)

(* Test 1: float# type *)
type chord_angle = float# [@@deriving unboxed_option]

module Chord_angle_option = Option

let () =
  let none = Chord_angle_option.none in
  assert (Chord_angle_option.is_none none);
  assert (not (Chord_angle_option.is_some none));
  let v = Chord_angle_option.some #3.14 in
  assert (Chord_angle_option.is_some v);
  assert (not (Chord_angle_option.is_none v));
  let default = #0.0 in
  let r = Chord_angle_option.value none ~default in
  assert (Float_u.equal r default);
  let r2 = Chord_angle_option.value v ~default in
  assert (Float_u.equal r2 #3.14);
  ()
;;

(* Test 2: match%optional_u for float# *)
let _test_optional_u (x : Chord_angle_option.t) =
  match%optional_u (x : Chord_angle_option.t) with
  | None -> #0.0
  | Some v -> v
;;

(* Test 3: unboxed record *)
type r2_point =
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
  assert (Float_u.equal v.#y #2.0);
  ()
;;

(* Test 4: value_exn raises on none *)
let () =
  let raised =
    try
      let _ = Chord_angle_option.value_exn Chord_angle_option.none in
      false
    with
    | Failure _ -> true
  in
  assert raised
;;

(* Test 5: int64# sentinel overrides accept unboxed literals *)
type int64_token = int64# [@@deriving unboxed_option { none = #0L }]

module Int64_token_option = Option

let () =
  let none = Int64_token_option.none in
  assert (Int64_token_option.is_none none);
  let v = Int64_token_option.some #7L in
  assert (Int64_token_option.is_some v);
  assert (not (Int64_token_option.is_none v));
  assert (Int64_u.equal (Int64_token_option.value none ~default:#9L) #9L);
  assert (Int64_u.equal (Int64_token_option.value_exn v) #7L)
;;

(* Test 6: int32# sentinel overrides accept unboxed literals *)
type int32_token = int32# [@@deriving unboxed_option { none = #0l }]

module Int32_token_option = Option

let () =
  assert (Int32_token_option.is_none Int32_token_option.none);
  let v = Int32_token_option.some #7l in
  assert (Int32_token_option.is_some v);
  assert (Int32_u.equal (Int32_token_option.value_exn v) #7l)
;;

(* Test 7: nativeint# sentinel overrides accept unboxed literals *)
type nativeint_token = nativeint# [@@deriving unboxed_option { none = #0n }]

module Nativeint_token_option = Option

let () =
  assert (Nativeint_token_option.is_none Nativeint_token_option.none);
  let v = Nativeint_token_option.some #7n in
  assert (Nativeint_token_option.is_some v);
  assert (Nativeint_u.equal (Nativeint_token_option.value_exn v) #7n)
;;

(* Test 8: float32# defaults to NaN sentinel *)
type float32_token = float32# [@@deriving unboxed_option]

module Float32_token_option = Option

let () =
  assert (Float32_token_option.is_none Float32_token_option.none);
  let v = Float32_token_option.some #1.25s in
  assert (Float32_token_option.is_some v);
  assert (Float32_u.equal (Float32_token_option.value_exn v) #1.25s)
;;

(* Test 9: int8# sentinel overrides accept unboxed literals *)
type int8_token = int8# [@@deriving unboxed_option { none = #0s }]

module Int8_token_option = Option

let () =
  assert (Int8_token_option.is_none Int8_token_option.none);
  let v = Int8_token_option.some #7s in
  assert (Int8_token_option.is_some v);
  assert (eq_int8_u (Int8_token_option.value_exn v) #7s)
;;

(* Test 10: int16# sentinel overrides accept unboxed literals *)
type int16_token = int16# [@@deriving unboxed_option { none = #0S }]

module Int16_token_option = Option

let () =
  assert (Int16_token_option.is_none Int16_token_option.none);
  let v = Int16_token_option.some #7S in
  assert (Int16_token_option.is_some v);
  assert (eq_int16_u (Int16_token_option.value_exn v) #7S)
;;

(* Test 11: int# sentinel overrides accept int literals *)
type int_token = int# [@@deriving unboxed_option { none = 0 }]

module Int_token_option = Option

let () =
  assert (Int_token_option.is_none Int_token_option.none);
  let v = Int_token_option.some (of_int_u 7) in
  assert (Int_token_option.is_some v);
  assert (eq_int_u (Int_token_option.value_exn v) (of_int_u 7))
;;

(* Test 12: char# sentinel overrides accept unboxed literals *)
type char_token = char# [@@deriving unboxed_option { none = #'\000' }]

module Char_token_option = Option

let () =
  assert (Char_token_option.is_none Char_token_option.none);
  let v = Char_token_option.some #'x' in
  assert (Char_token_option.is_some v);
  assert (eq_char_u (Char_token_option.value_exn v) #'x')
;;

(* Test 13: unboxed-record sentinels may use raw unboxed literals *)
type packed_point =
  #{ x : int8#
   ; y : int32#
   }
[@@deriving unboxed_option { none = #{ x = #0s; y = #0l } }]

module Packed_point_option = Option

let () =
  assert (Packed_point_option.is_none Packed_point_option.none);
  assert (Packed_point_option.is_none #{ x = #0s; y = #7l });
  assert (Packed_point_option.is_none #{ x = #7s; y = #0l });
  let p = Packed_point_option.some #{ x = #7s; y = #9l } in
  assert (Packed_point_option.is_some p);
  let v = Packed_point_option.value_exn p in
  assert (eq_int8_u v.#x #7s);
  assert (Int32_u.equal v.#y #9l)
;;
