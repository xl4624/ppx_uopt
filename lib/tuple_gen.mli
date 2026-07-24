(** AST generation helpers for unboxed-tuple payloads (see {!Uopt_types.Unboxed_tuple}). *)

open Ppxlib
open Uopt_types

(** Synthesized tuple sentinel for [Option.none] in sentinel mode. [none_override] must be
    [Some] of an unboxed-tuple literal with exactly [List.length kinds] unlabeled
    components (guaranteed by the caller: sentinel mode only fires when an override is
    present). Raises for a missing override, a labeled component, a mismatched arity, or a
    non-tuple-literal expression. *)
val gen_unboxed_tuple_none
  :  loc:location
  -> scalar_kind list
  -> none_override:expression option
  -> expression

(** [is_none] predicate for a sentinel-backed unboxed tuple: destructures positionally and
    conjoins a per-component check against [none_override] - see
    {!Scalar_gen.is_none_body}. *)
val gen_unboxed_tuple_is_none
  :  loc:location
  -> scalar_kind list
  -> none_override:expression option
  -> expression
  -> expression

(** Destructure an unboxed-tuple value and convert it to [Sexplib0.Sexp.t] as a [List] of
    its components' own sexp renditions (no field names to pair them with, matching plain
    OCaml tuples). *)
val gen_unboxed_tuple_sexp_of_value
  :  loc:location
  -> scalar_kind list
  -> expression
  -> expression
