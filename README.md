# ppx_uopt

`[@@deriving unboxed_option]` generates an allocation-free `Option` module for
unboxed scalar types and unboxed records in OxCaml.

`'a option` allocates a heap block on every `Some x`, and `'a or_null`
requires `'a : value` so it can't wrap unboxed types like `float#` or
`#{ ... }`. `ppx_uopt` does neither: `some x` is `#(true, x)` in tagged mode,
or `x` itself with one reserved sentinel value in sentinel mode.

## Installation

Requires an OxCaml switch.

```
opam pin add ppx_uopt git+https://github.com/xl4624/ppx_uopt.git
```

```dune
(library
 (name mylib)
 (preprocess (pps ppx_uopt)))
```

## Generated interface

```ocaml
module Option : sig
  type value = t
  type t  (* = value or #(bool * value), depending on representation *)

  val none            : t
  val some            : value -> t
  val is_none         : t -> bool
  val is_some         : t -> bool
  val value           : t -> default:value -> value
  val value_exn       : t -> value          (* raises Invalid_argument on none *)
  val unsafe_value    : t -> value          (* skip the is_none check *)

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none      : t -> bool  [@inline] [@zero_alloc]
      val unsafe_value : t -> value [@inline] [@zero_alloc]
    end
  end
end
```

The nested `Optional_syntax.Optional_syntax` module wires this type up to Jane
Street's [`[%optional]`](https://github.com/janestreet/ppx_optional) syntax
extension, if you use it.

## Supported types

Scalars: `float#`, `float32#`, `int32#`, `int64#`, `nativeint#`, `int8#`,
`int16#`, `int#`, `char#`.

Unboxed records, with any mix of scalars, immediates (`int`, `bool`, `char`,
`float`), contract types `M.t` (see [Contract fields](#contract-fields)), or any
other value-layout field.

## Representations

### Tagged (default)

When no `none = ...` is given, the deriver uses an unboxed bool-tagged pair,
`Option.t = #(bool * value)`:

```ocaml
type token = float# [@@deriving unboxed_option]
(* Option.none   = #(false, 0.) *)
(* Option.some v = #(true, v)   *)
```

`is_none` only inspects the bool tag, so the payload of `none` doesn't have to
be a special value. Tagged mode works on any record, including ones with
`string`, `array`, or other heap-allocated fields.

### Sentinel via `none = ...`

When you supply a `none = ...` override, `Option.t = value` directly - no extra
tag, no padding. You're reserving one specific value to mean "none":

```ocaml
type token = int8# [@@deriving unboxed_option { none = #0s }]
(* Option.none = #0s; Option.some is the identity *)
```

## Record examples

For records, write `none = #{ ... }`. Listed fields are `is_none`
discriminators; omitted fields are payload-only and `is_none` doesn't inspect
them. A single discriminator gives the simplest, fastest predicate.

```ocaml
type packed_pair = #{ x : int8#; y : int32# }
[@@deriving unboxed_option { none = #{ x = #12s; y = #0l } }]
(* is_none v = (v.#x = #12s) && (v.#y = #0l) - both must match *)
```

```ocaml
type packed_pair = #{ x : int8#; y : float# }
[@@deriving unboxed_option { none = #{ x = #15s } }]
(* is_none v = (v.#x = #15s); y can be anything - including nan *)
```

Opaque fields (anything that isn't a recognised scalar, immediate, or contract
type) are allowed too - omit them (payload-only) or list them (compared with
`Stdlib.( = )`):

```ocaml
type record = #{ id : int; tag : string }
[@@deriving unboxed_option { none = #{ id = -1 } }]
(* is_none v = (Stdlib.Int.equal v.#id (-1)); tag is payload-only *)
```

## Contract fields

A field typed `M.t` is supported when `M.Option` provides the right interface.
This lets you nest types that already have their own `Option` module without
re-deriving.

```ocaml
val M.Option.none         : M.Option.t
val M.Option.unsafe_value : M.Option.t -> M.t
val M.Option.is_none      : M.t -> bool   (* required only in sentinel mode *)
```

When `M` is itself `[@@deriving unboxed_option]`, this is automatic:

```ocaml
module Inner = struct
  type t = float# [@@deriving unboxed_option]
end

module Outer = struct
  type t = #{ inner : Inner.t; count : int# }
  [@@deriving unboxed_option]
end
```

For types that don't derive `unboxed_option`, hand-roll a compatible module (see
[test/test_contract_field.ml](./test/test_contract_field.ml) for a worked
example with a custom float wrapper).

## Aliases

`type t = M.t [@@deriving unboxed_option]` re-exports `M.Option` under the new
name. The alias path doesn't take a `none = ...` (delegation only).

```ocaml
type my_token = Some_module.t [@@deriving unboxed_option]
(* My_token.Option.t = Some_module.Option.t, all functions delegate. *)
```

See [test/](./test/) for more examples.
