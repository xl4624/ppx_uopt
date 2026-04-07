open Ppxlib
open Uopt_types

(** Parse-time classification of supported payload and field types. *)

(** Return the supported scalar kind for [ct], or [None] if [ct] is not one of the unboxed
    scalar types handled by this deriver. *)
val scalar_kind_of_core_type : core_type -> scalar_kind option

(** Classify an unboxed-record field as either a built-in scalar field or a delegated
    contract field of the form [M.t].

    Raises a location error for unsupported field shapes. *)
val classify_record_field : loc:location -> label_declaration -> record_field_kind

(** Classify the declaration being derived.

    Supported declarations are scalar manifest types and unboxed record products. Raises a
    location error for unsupported forms. *)
val detect_type_info : loc:location -> type_declaration -> type_info
