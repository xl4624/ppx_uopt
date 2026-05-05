(** AST generation helpers for unboxed-record payloads. *)

open Ppxlib

(** Generated local binding name for the default payload extracted from a delegated
    contract field's [Option.none]. *)
val contract_payload_name : string -> string

(** Generated local binding name for a delegated contract field's [Option.is_none]. *)
val contract_is_none_name : string -> string

(** Type-specialised equality function for an immediate field. *)
val immediate_equal_fn : loc:location -> Uopt_types.immediate_kind -> expression

(** Default never-observed value for an immediate field omitted from a sentinel override. *)
val immediate_default_expr : loc:location -> Uopt_types.immediate_kind -> expression

(** Parse [none = #{ ... }] overrides for an unboxed record.

    Returns the explicitly provided field expressions as [(field_name, expr)] pairs.
    Omitted fields are not included and will later fall back to synthesized defaults.
    Raises a location error for non-record expressions, [with]-updates, duplicate fields,
    or unknown field syntax. *)
val unboxed_record_none_overrides
  :  loc:location
  -> expression option
  -> (string * expression) list

(** Build the synthesized record sentinel used for [Option.none].

    Explicitly overridden fields use their provided expressions. Omitted scalar fields use
    their scalar default sentinel, omitted contract fields reuse the payload extracted
    from the field contract's own [Option.none], omitted immediate fields use a literal
    default, and omitted opaque fields fall back to a never-observed [Obj.magic 0]
    placeholder. Omitted fields are payload-only - [is_none] does not inspect them. *)
val gen_unboxed_record_none
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> expression

(** Generate helper bindings needed by record code generation.

    Contract fields contribute a payload binding extracted from [M.Option.none]. When
    [need_is_none] is [true], this also binds [M.Option.is_none] for each contract field
    so sentinel-backed record predicates can delegate field checks. *)
val contract_helper_items
  :  loc:location
  -> label_declaration list
  -> need_is_none:bool
  -> structure_item list

(** Whether the sentinel-mode [is_none] predicate generated for [labels] under
    [none_override] would use polymorphic equality on at least one opaque field. Such
    [is_none] bodies cannot be marked [@@zero_alloc]; callers gate the assume-style
    annotation on this. *)
val unboxed_record_is_none_uses_poly_eq
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> bool

(** Build the [is_none] predicate for a sentinel-backed unboxed record.

    Only fields listed in [none = #{ ... }] are checked: the predicate is the conjunction
    of per-field equality tests against the user-supplied sentinel value. Omitted fields
    are payload-only and may freely take any value. Immediate fields ([int]/[bool]/[char])
    use type-specialised equality; opaque fields use [Stdlib.( = )]. *)
val gen_unboxed_record_is_none_sentinel
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> expression
  -> expression
