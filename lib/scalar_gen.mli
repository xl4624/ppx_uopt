(** AST generation helpers for scalar payload representations. *)

open Ppxlib
open Uopt_types

(** User-facing name for a supported scalar kind, used in diagnostics. *)
val kind_name : scalar_kind -> string

(** Built-in sentinel for [kind], if one exists: NaN for floats, the minimum representable
    value for integers, [None] for [char#]. *)
val default_none_expr : loc:location -> scalar_kind -> expression option

(** Sentinel expression for [none]: [none_override] normalized to the unboxed literal form
    if present, else the built-in default. Raises when [kind] has neither. *)
val none_expr
  :  loc:location
  -> kind:scalar_kind
  -> none_override:expression option
  -> expression

(** Default payload for the tagged representation - fills the [#(false, payload)] slot;
    need not be a reserved sentinel. *)
val default_payload_expr : loc:location -> scalar_kind -> expression

(** [is_none] predicate for a scalar expression: a typed equality against {!none_expr}'s
    sentinel (or [is_nan], when the sentinel is syntactically NaN). *)
val is_none_body
  :  loc:location
  -> kind:scalar_kind
  -> none_override:expression option
  -> expression
  -> expression

(** Convert a scalar value to [Sexplib0.Sexp.t]. *)
val sexp_of_value_expr : loc:location -> scalar_kind -> expression -> expression

(** Primitive external requested by a scalar kind. Opaque so callers aggregating across
    multiple kinds (e.g. a record with both [int8#] and [char#] fields) can dedup shared
    primitives like [_uopt_equal_int8] via {!compare_primitive}. *)
type primitive

val compare_primitive : primitive -> primitive -> int

(** Emit the [external] declaration for a primitive. *)
val primitive_item : loc:location -> primitive -> structure_item

(** Primitives needed by generated code for [kind]. *)
val primitives_for_kind : scalar_kind -> primitive list

(** Non-primitive helper [let] bindings needed by generated code for [kind]; unlike
    primitives, these don't need deduplication. *)
val extra_bindings : loc:location -> scalar_kind -> structure_item list
