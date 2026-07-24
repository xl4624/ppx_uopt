(** Parse-time classification of supported payload and field types. *)

open Ppxlib
open Uopt_types

(** Classify an unboxed-record field: a recognised unboxed scalar, an immediate
    ([int]/[bool]/[char]), a module-qualified contract type [M.t], or - anything else -
    opaque (the field's syntactic type, for a tagged-mode [Obj.magic 0] placeholder).

    A field whose type is syntactically an unboxed tuple (e.g. [#(int# * float#)]) is
    rejected instead: it's provably not value-layout, so it can't stand in as an opaque
    placeholder. Named types that are non-value-layout under the hood (e.g. an alias for
    another unboxed record) can't be caught here, since the ppx runs before type-checking;
    such a field surfaces as a layout-mismatch error at the placeholder site instead. *)
val classify_record_field : loc:location -> label_declaration -> record_field_kind

(** Classify the type declaration being derived: a scalar manifest, an unboxed-tuple
    manifest of scalars, or an unboxed record product. Raises for unsupported forms. *)
val detect_type_info : loc:location -> type_declaration -> type_info

(** Classify the declaration for signature generation. Unlike {!detect_type_info}, accepts
    an abstract type with just a jkind annotation (e.g. [type t : float64]), since a
    signature doesn't need the payload's internal structure. Returns [`Alias base] for a
    manifest of the form [M.t], otherwise [`Payload]. Raises for an abstract type with
    neither a manifest nor a jkind annotation. *)
val detect_sig_info
  :  loc:location
  -> type_declaration
  -> [ `Alias of Longident.t | `Payload ]
