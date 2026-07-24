(** Classification ADTs shared by the code generators: normalize the subset of OxCaml
    types supported by [@@deriving unboxed_option] so later stages don't repeatedly
    inspect the parsetree. *)

open Ppxlib

(** The concrete representation chosen for the derived option type. *)
type repr_kind =
  | Sentinel_repr (** Use a reserved payload sentinel to encode [none]. *)
  | Tagged_repr (** Store an explicit [bool] tag alongside the payload. *)

(** Supported scalar payload kinds. *)
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

(** Total ordering on [scalar_kind], used by generated code and tests. *)
val compare_scalar_kind : scalar_kind -> scalar_kind -> int

(** Boxed OCaml types with a typed, statically zero-alloc primitive equality ([Int.equal]
    etc.). [Imm_float] additionally gets NaN detection: an override of [Float.nan]
    generates [Float.is_nan] rather than [Float.equal], which is always [false] against
    NaN. *)
type immediate_kind =
  | Imm_int
  | Imm_bool
  | Imm_char
  | Imm_float

(** Classification of a field in an unboxed record payload. *)
type record_field_kind =
  | Record_field_scalar of scalar_kind
  | Record_field_contract of Longident.t
  (** A field delegated to an existing [M.Option] contract for [M.t]. *)
  | Record_field_immediate of immediate_kind
  | Record_field_opaque of core_type
  (** Neither a scalar, immediate, nor [M.t] contract; carries the field's syntactic type.
      Tagged mode materialises [(Stdlib.Obj.magic 0 : <core_type>)] - safe since the
      payload is never observed by [is_none]. In sentinel mode the field may appear in
      [none = #{ ... }] (compared with [Stdlib.( = )]) or be omitted, payload-only. *)

(** Classification of the payload portion of a type declaration (excludes aliases). *)
type payload_type_info =
  | Scalar of scalar_kind
  | Unboxed_tuple of scalar_kind list
  (** [#(ty1 * ty2 * ...)] of recognised scalars. No field names, so every component is
      always an [is_none] discriminator and [none = #( ... )] must supply all of them. *)
  | Unboxed_record of label_declaration list

(** Classification of the user-written type declaration being derived. *)
type type_info =
  | Payload of payload_type_info
  | Alias of Longident.t (** [type t = M.t], which delegates to [M.Option]. *)
