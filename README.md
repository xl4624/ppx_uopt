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
module Option : sig
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
end
```

## Supported types

Scalars: `float#`, `float32#`, `int32#`, `int64#`, `nativeint#`, `int8#`, `int16#`,
`int#`, `char#`

Unboxed records. Tagged mode supports records with arbitrary fields. Sentinel
mode requires each field to be a supported scalar or a module-qualified
contract field of the form `M.t` (see [Contract fields](#contract-fields)),
unless an explicit override is given (e.g. `none = #{ x = 0 }`).

## Representations

### Tagged (default)

When no `none = ...` override is given, `Option.t = #(bool * value)`:

```ocaml
type token = float# [@@deriving unboxed_option]
(* Option.t    = #(bool * float#) *)
(* Option.none = #(false, 0.)     *)
(* Option.some v = #(true, v)     *)
```

`is_none` only checks the leading tag; the payload is irrelevant.

### Sentinel via `none = ...`

`Option.t = value`, with a reserved sentinel:

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

Partial override - the listed fields are the `is_none` discriminators; omitted
fields are payload-only:

```ocaml
type packed_pair = #{ x : int8#; y : float# }
[@@deriving unboxed_option { none = #{ x = #15s } }]
(* synthesized none = #{ x = #15s; y = Float_u.nan () } *)
(* is_none v = (v.#x = #15s) - y can be anything *)
(* #{ x = #15s; y = #7.0 } is none *)
(* #{ x = #1s;  y = nan }  is some *)
```

This means **write only the field that discriminates and you get a single
typed compare**. Use a full override to check every field:

```ocaml
type packed_pair = #{ x : int8#; y : int32# }
[@@deriving unboxed_option { none = #{ x = #12s; y = #0l } }]
(* is_none v = (v.#x = #12s) && (v.#y = #0l) - both must match *)
```

Override on an opaque field - any field whose type isn't a recognised scalar
or contract `M.t` works in sentinel mode as long as the user provides its
sentinel value. The opaque field is compared with `Stdlib.( = )` (polymorphic
equality), which is fine for primitive immediates and small structural values
but lowers to `caml_equal` for arrays / nested records:

```ocaml
type record = #{ id : int; tag : string }
[@@deriving unboxed_option { none = #{ id = -1 } }]
(* synthesized none.#tag = (Stdlib.Obj.magic 0 : string), never observed *)
(* is_none v = (v.#id = -1) - tag is payload-only *)
```

If a discriminator's field type is an immediate (`int`, `bool`, `char`) or
unboxed scalar (`int#`, `float#`, etc.), the generated `is_none` is statically
`[@@zero_alloc]`. Multi-field overrides that include an opaque structural type
(e.g. an `int array`) fall back to `caml_equal` which the static checker can't
verify - prefer a single-field discriminator on a primitive when you want
guaranteed zero-alloc.

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
  Listed fields are the `is_none` discriminators; omitted fields are payload-only.
- **`sentinel = true`** - sentinel-backed with synthesized defaults (floats only). For
  records this checks every field against its synthesized default; use it when every
  field is genuinely a discriminator.
- **Single-field discriminator** - the simplest path for records: write
  `none = #{ id = ... }` naming just the discriminator. `is_none` becomes one typed
  compare and is statically `[@@zero_alloc]`.

See [test/](./test/) for more examples.
