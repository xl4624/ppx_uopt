(** AST generation helpers for unboxed-record payloads. *)

open Ppxlib
open Uopt_types

(** Generated local binding name for the default payload extracted from a delegated
    contract field's [Option.none]. *)
val contract_payload_name : string -> string

(** Generated local binding name for a delegated contract field's [Option.is_none]. *)
val contract_is_none_name : string -> string

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
    their scalar default sentinel, and omitted contract fields reuse the payload extracted
    from the field contract's own [Option.none]. *)
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

(** Build the [is_none] predicate for a sentinel-backed unboxed record.

    Every field is checked against that field's sentinel. With a partial [none = #{ ... }]
    override, explicitly listed fields use the override while omitted fields still use
    their synthesized default sentinels. *)
val gen_unboxed_record_is_none_sentinel
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> expression
  -> expression
