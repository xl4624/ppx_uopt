open Ppxlib

(** Internal classifications shared by the code generators.

    These types normalize the subset of OxCaml types supported by [@@deriving
    unboxed_option] so later stages can generate code without repeatedly inspecting the
    parsetree. *)

(** The concrete representation chosen for the derived option type. *)
type repr_kind =
  (** Use a reserved payload sentinel to encode [none]. *)
  | Sentinel_repr
  (** Store an explicit [bool] tag alongside the payload. *)
  | Tagged_repr

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

(** Classification of a field in an unboxed record payload. *)
type record_field_kind =
  (** A field whose representation is generated directly by this deriver. *)
  | Record_field_scalar of scalar_kind
  (** A field delegated to an existing [M.Option] contract for [M.t]. *)
  | Record_field_contract of Longident.t

(** Classification of the user-written type declaration being derived. *)
type type_info =
  (** A supported scalar manifest type such as [float#] or [int32#]. *)
  | Scalar of scalar_kind
  (** An unboxed record product whose fields are individually classified. *)
  | Unboxed_record of label_declaration list
