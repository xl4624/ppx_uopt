# ppx_uopt

`ppx_uopt` derives allocation-free `Option`-style helpers for unboxed scalar types and
unboxed records in OxCaml.

It generates an `Option` module with:
- `none`
- `some`
- `is_none`
- `is_some`
- `value`
- `value_exn`
- `unchecked_value`
- `Optional_syntax`

## Supported Types

Supported scalar types:
- `float#`
- `float32#`
- `int32#`
- `int64#`
- `bits64`
- `nativeint#`
- `int8#`
- `int16#`
- `int#`
- `char#`

Supported record types:
- unboxed records whose fields are all either supported unboxed scalar fields
- or module-qualified contract fields of the form `M.t`

## Representation

`ppx_uopt` has two representations.

Without `none = ...`:
- `Option.t = #(bool * value)`
- `Option.none = #(false, payload)`
- `Option.some v = #(true, v)`
- `Option.is_none` only checks whether the leading `bool` is `false`

With `none = ...`:
- `Option.t = value`
- `Option.none = sentinel`
- `Option.some` is the identity
- `Option.is_none` uses sentinel checks

With `sentinel = true`:
- `Option.t = value`
- `Option.none` is synthesized from the default sentinel for the type
- `Option.some` is the identity
- `Option.is_none` uses the synthesized sentinel checks

## Contract Fields

A record field of type `M.t` is supported only when the generated code can rely on this
contract:

```ocaml
M.Option.none : M.Option.t
M.Option.unchecked_value : M.Option.t -> M.t
```

If the enclosing derived option uses an explicit `none = ...` sentinel, generated record
`is_none` checks also require:

```ocaml
M.Option.is_none : M.t -> bool
```

So for `M.t` fields, generated code may refer to:
- `M.Option.none`
- `M.Option.unchecked_value`
- `M.Option.is_none` in sentinel-backed record mode

## Examples

Tagged scalar:

```ocaml
type token = float# [@@deriving unboxed_option]
```

Sentinel-backed scalar:

```ocaml
type token = int8# [@@deriving unboxed_option { none = #0s }]
```

Sentinel-backed scalar using the default sentinel:

```ocaml
type token = float# [@@deriving unboxed_option { sentinel = true }]
```

Sentinel-backed record:

```ocaml
type packed_pair =
  #{ x : int8#
   ; y : int32#
   }
[@@deriving unboxed_option { none = #{ x = #12s; y = #0l } }]
```

Sentinel-backed record with a partial override:

```ocaml
type packed_pair =
  #{ x : int8#
   ; y : float#
   }
[@@deriving unboxed_option { none = #{ x = #15s } }]
```

Omitted fields in a record `none = #{ ... }` override use their default sentinels.
In this example the synthesized `none` value is `#{ x = #15s; y = Float_u.nan () }`.
`Option.is_none` must still check both fields, so `#{ x = #15s; y = #7.0 }` is `some`,
not `none`.

Record with a nested contract field:

```ocaml
module Foo = struct
  type t = float#

  module Option = struct
    type value = t
    type t = #(bool * value)

    let none = #(false, Float_u.nan ())

    let unchecked_value = function
      | #(_, value) -> value
  end
end

type record = #{ x : Foo.t } [@@deriving unboxed_option]
```

See more examples in [test/](./test/).

## Record Semantics

Tagged mode: a record option is `none` only when its outer tag is `false`

Sentinel mode: a record option is `none` iff every field matches that field's sentinel.
With `none = #{ ... }`, explicitly listed fields use the provided values and omitted fields
still use their synthesized default sentinels.

## Notes

- Use tagged mode when you want explicit semantics and do not want correctness to depend
  on reserving a sentinel payload.
- Use `none = ...` when you have a value that can be reserved as a sentinel and you
  intentionally want the more compact sentinel-backed representation.
- Use `sentinel = true` when you want sentinel-backed representation with synthesized
  default sentinels.
- Unboxed records may use `none = #{ ... }` to define or partially override a record
  sentinel explicitly.

## TODO

- Improve diagnostics around contract fields.
  The current behavior is explicit, but the error messages could do a better job
  of explaining when a field is rejected because it is not written as `M.t`, and
  when generated code later fails because `M.Option.none`,
  `M.Option.unchecked_value`, or `M.Option.is_none` is missing.
