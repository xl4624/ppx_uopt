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
  | Record_field_opaque of core_type
  (** Field whose type is neither a recognised unboxed scalar nor a module-qualified
      contract type. The carried [core_type] is the field's original type expression, used
      as an explicit annotation on the tagged-mode payload placeholder. *)

type payload_type_info =
  | Scalar of scalar_kind
  | Unboxed_record of label_declaration list

type type_info =
  | Payload of payload_type_info
  | Alias of Longident.t
