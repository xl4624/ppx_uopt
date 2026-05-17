(** Small AST construction helpers used across the generators.

    These functions keep the code generators focused on representation logic rather than
    repetitive [Ppxlib] boilerplate. *)

open Ppxlib

(** Build a value binding named [name], automatically marking the body [@inline] and
    [@zero_alloc] (or [@zero_alloc assume] when [assume_zero_alloc] is true). The [assume]
    variant tells the checker to trust the annotation rather than verify it; used for
    bodies whose actual runtime cost is zero-alloc but whose static analysis isn't (e.g.
    [Stdlib.( = )] which lowers to [caml_equal]). *)
val mk_val_binding
  :  ?assume_zero_alloc:bool
  -> loc:location
  -> string
  -> expression
  -> value_binding

(** Emit [let name = body] as a top-level non-recursive structure item, with no extra
    attributes. *)
val let_def : loc:location -> string -> expression -> structure_item

(** Like {!let_def} but binds an arbitrary pattern, used for bindings that need a type
    ascription. *)
val let_def_pat : loc:location -> pattern -> expression -> structure_item

(** Like {!let_def}, but additionally marks the body [@inline] and [@zero_alloc] (see
    {!mk_val_binding} for the [assume_zero_alloc] flag). *)
val let_inline
  :  ?assume_zero_alloc:bool
  -> loc:location
  -> string
  -> expression
  -> structure_item

(** The tagged representation type [#(bool * value)]. *)
val tagged_option_type : loc:location -> core_type

(** Tagged [none] expression [#(false, payload)]. *)
val tagged_none_expr : loc:location -> expression -> expression

(** Tagged [some] expression [#(true, payload)]. *)
val tagged_some_expr : loc:location -> expression -> expression

(** Qualified identifier expression formed by extending an existing [Longident.t]. For
    static paths, prefer [Ppxlib.Ast_builder.Default.evar ~loc "A.B.c"] which parses the
    dot-separated string. *)
val qual_evar : loc:location -> Longident.t -> string list -> expression

(** Convert boxed numeric and character constants into their unboxed literal forms when
    possible. Non-constant expressions are returned unchanged. *)
val to_unboxed_constant_expr : loc:location -> expression -> expression

(** Build [(Stdlib.Obj.magic 0 : <field_type>)] for use as a never-observed placeholder
    payload in opaque value-layout fields. *)
val opaque_default_payload_expr : loc:location -> core_type -> expression

(** True iff [expr] is a syntactic identifier reference matching one of [paths] (e.g.
    ["Float_u.nan"]). Used by NaN detection in float-kind sentinel overrides. *)
val expr_is_qualified_ident : loc:location -> expression -> string list -> bool

(** [@alloc a @ m = (heap @ global, stack @ local)] template attribute. *)
val alloc_heap_stack_attr : loc:location -> attribute

(** [@zero_alloc ignore] attribute. *)
val zero_alloc_ignore_attr : loc:location -> attribute

(** [@alloc a] attribute (for attaching to a function ident during application). *)
val alloc_var_attr : loc:location -> attribute

(** [@exclave_if_stack a] attribute (for attaching to an expression returning the result). *)
val exclave_if_stack_attr : loc:location -> attribute

(** Attach [@exclave_if_stack a] to an expression. *)
val with_exclave_if_stack : loc:location -> expression -> expression

(** Attach [@alloc a] to an expression (typically a function identifier). *)
val with_alloc_var : loc:location -> expression -> expression

(** Wrap a structure item in [Pstr_extension ("template", ...)], i.e. [let%template ...]. *)
val pstr_template : loc:location -> structure_item -> structure_item

(** Wrap a signature item in [Psig_extension ("template", ...)], i.e. [val%template ...]. *)
val psig_template : loc:location -> signature_item -> signature_item

(** Build a
    [val%template name : arg_type @ m -> result_type @ m [\@\@alloc a \@ m = (heap \@ global, stack \@ local)]]
    signature item. *)
val templated_heap_stack_sig_value
  :  loc:location
  -> name:string
  -> arg_type:core_type
  -> result_type:core_type
  -> signature_item
