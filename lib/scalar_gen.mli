(** AST generation helpers for scalar payload representations. *)

open Ppxlib
open Uopt_types

(** User-facing name for a supported scalar kind, used in diagnostics. *)
val kind_name : scalar_kind -> string

(** Built-in sentinel for [kind], if one exists.

    Floating-point kinds use NaN. Integer kinds use their minimum representable value.
    [char#] has no built-in sentinel and returns [None]. *)
val default_none_expr : loc:location -> scalar_kind -> expression option

(** Sentinel expression used to represent [none] for [kind].

    When [none_override] is present it is normalized to the appropriate unboxed literal
    form; otherwise the built-in default sentinel is used. Raises a location error when
    [kind] has no default sentinel and no override is supplied. *)
val none_expr
  :  loc:location
  -> kind:scalar_kind
  -> none_override:expression option
  -> expression

(** Default payload used in tagged representations.

    This value only fills the payload slot of [#(false, payload)] and does not need to be
    a reserved sentinel. *)
val default_payload_expr : loc:location -> scalar_kind -> expression

(** Build the [is_none] predicate for a scalar expression as a typed equality against the
    sentinel expression returned by {!none_expr}. *)
val is_none_body
  :  loc:location
  -> kind:scalar_kind
  -> none_override:expression option
  -> expression
  -> expression

(** Build an expression that converts a scalar value to [Sexplib0.Sexp.t]. *)
val sexp_of_value_expr : loc:location -> scalar_kind -> expression -> expression

(** Primitive externals requested by a scalar kind. Kept as an opaque enum so that callers
    aggregating across multiple kinds (e.g. an unboxed record with both [int8#] and
    [char#] fields) can deduplicate shared primitives like [_uopt_equal_int8]. *)
type primitive

(** Total ordering on [primitive], used by callers to deduplicate via [List.sort_uniq]. *)
val compare_primitive : primitive -> primitive -> int

(** Emit the [external] declaration for a primitive. *)
val primitive_item : loc:location -> primitive -> structure_item

(** Primitives needed by generated code for [kind]. *)
val primitives_for_kind : scalar_kind -> primitive list

(** Non-primitive helper bindings (regular [let]s) needed by generated code for [kind].
    These do not need deduplication: each kind's extras are unique to that kind. *)
val extra_bindings : loc:location -> scalar_kind -> structure_item list
