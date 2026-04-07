open Ppxlib
open Uopt_types

(** AST generation helpers for scalar payload representations. *)

(** User-facing name for a supported scalar kind, used in diagnostics. *)
val kind_name : scalar_kind -> string

(** Built-in sentinel for [kind], if one exists.

    Floating-point kinds use NaN. Integer-like kinds require an explicit [none = ...]
    override and therefore return [None]. *)
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

(** Build the [is_none] predicate for a scalar expression.

    Floating-point sentinels use [Float_u.is_nan]/[Float32_u.is_nan] when no explicit
    override is given; all other cases compare against [none_expr]. *)
val is_none_body
  :  loc:location
  -> kind:scalar_kind
  -> none_override:expression option
  -> expression
  -> expression

(** Helper definitions required by generated scalar option modules for [kind].

    This emits primitives only for scalar kinds that need extra support code, such as
    unboxed [int#], [int8#], [int16#], and [char#]. *)
val helper_items : loc:location -> scalar_kind -> structure_item list
