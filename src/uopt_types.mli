(** Internal classifications shared by the code generators.

    These types normalize the subset of OxCaml types supported by
    [@@deriving unboxed_option] so later stages can generate code without repeatedly
    inspecting the parsetree. *)

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

(** Classification of a field in an unboxed record payload. *)
type record_field_kind =
  | Record_field_scalar of scalar_kind
  (** A field whose representation is generated directly by this deriver. *)
  | Record_field_contract of Longident.t
  (** A field delegated to an existing [M.Option] contract for [M.t]. *)

(** Classification of the payload portion of a type declaration (excludes aliases). *)
type payload_type_info =
  | Scalar of scalar_kind
  (** A supported scalar manifest type such as [float#] or [int32#]. *)
  | Unboxed_record of label_declaration list
  (** An unboxed record product whose fields are individually classified. *)

(** Classification of the user-written type declaration being derived. *)
type type_info =
  | Payload of payload_type_info
  | Alias of Longident.t (** A type alias [type t = M.t] that delegates to [M.Option]. *)
