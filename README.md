# ppx_uopt

`[@@deriving unboxed_option]` generates an allocation-free `Option` module for unboxed
scalar types and unboxed records in OxCaml.

## Installation

Requires an OxCaml switch. To install:

```
opam pin add ppx_uopt git+https://github.com/xl4624/ppx_uopt.git
```

Then add `ppx_uopt` to your library's `preprocess` stanza:

```dune
(library
 (name mylib)
 (preprocess (pps ppx_uopt)))
```

## Generated interface

```ocaml
type value = t
type t  (* = value or #(bool * value), depending on representation *)

val none            : t
val some            : value -> t
val is_none         : t -> bool
val is_some         : t -> bool
val value           : t -> default:value -> value
val value_exn       : t -> value
val unchecked_value : t -> value

module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none      : t -> bool  [@inline] [@zero_alloc]
    val unsafe_value : t -> value [@inline] [@zero_alloc]
  end
end
```

## Supported types

Scalars: `float#`, `float32#`, `int32#`, `int64#`, `nativeint#`, `int8#`, `int16#`,
`int#`, `char#`

Unboxed records whose fields are all either supported scalars or module-qualified contract
fields of the form `M.t` (see [Contract fields](#contract-fields)).

## Representations

### Tagged (default)

When no `none = ...` override is given, `t = #(bool * value)`:

```ocaml
type token = float# [@@deriving unboxed_option]
(* Option.t    = #(bool * float#) *)
(* Option.none = #(false, 0.)     *)
(* Option.some v = #(true, v)     *)
```

`is_none` only checks the leading tag; the payload is irrelevant.

### Sentinel via `none = ...`

`t = value`, with a reserved sentinel:

```ocaml
type token = int8# [@@deriving unboxed_option { none = #0s }]
(* Option.t    = int8# *)
(* Option.none = #0s   *)
(* Option.some is the identity *)
```

### Sentinel via `sentinel = true`

Uses a synthesized default sentinel (NaN for floats; not available for integer types):

```ocaml
type token = float# [@@deriving unboxed_option { sentinel = true }]
```

## Record examples

Full sentinel override:

```ocaml
type packed_pair = #{ x : int8#; y : int32# }
[@@deriving unboxed_option { none = #{ x = #12s; y = #0l } }]
```

Partial override - omitted fields use their default sentinels:

```ocaml
type packed_pair = #{ x : int8#; y : float# }
[@@deriving unboxed_option { none = #{ x = #15s } }]
(* synthesized none = #{ x = #15s; y = Float_u.nan () } *)
(* is_none checks BOTH fields: #{ x = #15s; y = #7.0 } is some, not none *)
```

## Contract fields

A record field of type `M.t` is supported when the generated code can satisfy this
contract:

```ocaml
M.Option.none            : M.Option.t
M.Option.unchecked_value : M.Option.t -> M.t
```

In sentinel-backed record mode, `is_none` also requires:

```ocaml
M.Option.is_none : M.t -> bool
```

Example:

```ocaml
module Foo = struct
  type t = float#
  module Option = struct
    type value = t
    type t = #(bool * value)
    let none = #(false, Float_u.nan ())
    let unchecked_value = function #(_, v) -> v
  end
end

type record = #{ x : Foo.t } [@@deriving unboxed_option]
```

## Guidance

- **Tagged mode** - explicit semantics; correctness does not depend on a reserved sentinel.
- **`none = ...`** - compact representation when a specific value can be safely reserved.
- **`sentinel = true`** - sentinel-backed with synthesized defaults (floats only).
- **Partial record `none`** - `is_none` still checks every field, so only the exact
  synthesized sentinel is `none`.

See [test/](./test/) for more examples.
