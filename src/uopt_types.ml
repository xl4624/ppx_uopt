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

type record_field_kind =
  | Record_field_scalar of scalar_kind
  | Record_field_contract of Longident.t

type type_info =
  | Scalar of scalar_kind
  | Unboxed_record of label_declaration list
