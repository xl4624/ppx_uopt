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

type immediate_kind =
  | Imm_int
  | Imm_bool
  | Imm_char
  | Imm_float

type record_field_kind =
  | Record_field_scalar of scalar_kind
  | Record_field_contract of Longident.t
  | Record_field_immediate of immediate_kind
  | Record_field_opaque of core_type

type payload_type_info =
  | Scalar of scalar_kind
  | Unboxed_tuple of scalar_kind list
  | Unboxed_record of label_declaration list

type type_info =
  | Payload of payload_type_info
  | Alias of Longident.t
