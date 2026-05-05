open Ppxlib

type repr_kind =
  | Sentinel_repr
  | Tagged_repr

type scalar_kind =
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar
  | Char_u_scalar

let compare_scalar_kind a b = Stdlib.compare a b

(** Boxed-OCaml types with a typed primitive equality function ({!Stdlib.Int.equal} etc.).
    Treated specially because their equality is verified zero-alloc, and because [Imm_float]
    additionally gets NaN-detection: an override of [Float.nan] generates [Float.is_nan]
    rather than [Float.equal] (which is always [false] against NaN by IEEE 754). *)
type immediate_kind =
  | Imm_int
  | Imm_bool
  | Imm_char
  | Imm_float

type record_field_kind =
  | Record_field_scalar of scalar_kind
  | Record_field_contract of Longident.t
  | Record_field_immediate of immediate_kind
  (** Field whose type is an OCaml immediate ([int], [bool], [char]). [is_none] uses the
      type-specialised [Int.equal]/[Bool.equal]/[Char.equal] which is statically verified
      zero-alloc. *)
  | Record_field_opaque of core_type
  (** Field whose type is neither a recognised unboxed scalar, immediate, nor a
      module-qualified contract type. The carried [core_type] is the field's original type
      expression, used as an explicit annotation on the tagged-mode payload placeholder.
      In sentinel mode the field may appear in the [none = #{ ... }] override (compared
      with [Stdlib.( = )]) or be omitted, in which case it is payload-only. *)

type payload_type_info =
  | Scalar of scalar_kind
  | Unboxed_record of label_declaration list

type type_info =
  | Payload of payload_type_info
  | Alias of Longident.t
