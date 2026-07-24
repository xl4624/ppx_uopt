(** AST generation helpers for unboxed-record payloads. *)

open Ppxlib

(** Generated local binding name for the default payload extracted from a delegated
    contract field's [Option.none]. *)
val contract_payload_name : string -> string

(** Default never-observed value for an immediate field omitted from a sentinel override. *)
val immediate_default_expr : loc:location -> Uopt_types.immediate_kind -> expression

(** Build the synthesized record sentinel used for [Option.none]. Omitted fields fall back
    to a per-kind default (scalar sentinel, contract's own [Option.none] payload,
    immediate literal, or [Obj.magic 0] for opaque) and are payload-only - [is_none] does
    not inspect them. *)
val gen_unboxed_record_none
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> expression

(** Local bindings needed by record code generation: contract fields get a payload
    extracted from [M.Option.none], plus [M.Option.is_none] when [need_is_none]. *)
val contract_helper_items
  :  loc:location
  -> label_declaration list
  -> need_is_none:bool
  -> structure_item list

(** Whether sentinel-mode [is_none] for [labels] under [none_override] uses polymorphic
    equality on an opaque field. Such bodies can't be statically proven [@@zero_alloc];
    callers gate the [assume] annotation on this. *)
val unboxed_record_is_none_uses_poly_eq
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> bool

(** Build the [is_none] predicate for a sentinel-backed unboxed record: the conjunction of
    per-field equality checks for only the fields listed in [none = #{ ... }]. Omitted
    fields are payload-only and may freely take any value. *)
val gen_unboxed_record_is_none_sentinel
  :  loc:location
  -> label_declaration list
  -> none_override:expression option
  -> expression
  -> expression
