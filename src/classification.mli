(** Parse-time classification of supported payload and field types. *)

open Ppxlib
open Uopt_types

(** Return the supported scalar kind for [ct], or [None] if [ct] is not one of the unboxed
    scalar types handled by this deriver. *)
val scalar_kind_of_core_type : core_type -> scalar_kind option

(** Classify an unboxed-record field. Recognised shapes are unboxed scalar types
    ({!scalar_kind_of_core_type}) and module-qualified contract types of the form [M.t].
    Anything else is reported as {!Record_field_opaque} with the field's syntactic type so
    the tagged-mode generator can build a [(Obj.magic 0 : <type>)] placeholder;
    sentinel-mode generators raise on that variant because they have no equality function
    to use. *)
val classify_record_field : loc:location -> label_declaration -> record_field_kind

(** Classify the declaration being derived.

    Supported declarations are scalar manifest types and unboxed record products. Raises a
    location error for unsupported forms. *)
val detect_type_info : loc:location -> type_declaration -> type_info

(** Classify the declaration for signature generation.

    Signatures don't need to know the internal structure of a payload, so this accepts
    abstract types with only a jkind annotation (e.g. [type t : float64]). Returns
    [`Alias base] when the declaration is a manifest of the form [M.t], otherwise
    [`Payload]. Raises for abstract types with neither a manifest nor a jkind annotation. *)
val detect_sig_info
  :  loc:location
  -> type_declaration
  -> [ `Alias of Longident.t | `Payload ]
