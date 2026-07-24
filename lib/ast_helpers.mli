(** Small AST construction helpers used across the generators. *)

open Ppxlib

(** [let name = body], marked [@inline] and [@zero_alloc] (or [@zero_alloc assume] when
    [assume_zero_alloc] is true - trust rather than verify, e.g. for [Stdlib.( = )] which
    lowers to [caml_equal]). *)
val let_inline
  :  ?assume_zero_alloc:bool
  -> loc:location
  -> string
  -> expression
  -> structure_item

(** [let name = body] with no attributes. *)
val let_def : loc:location -> string -> expression -> structure_item

(** Like {!let_def} but with an arbitrary pattern, for bindings that need a type
    ascription. *)
val let_def_pat : loc:location -> pattern -> expression -> structure_item

(** The tagged representation type [#(bool * value)]. *)
val tagged_option_type : loc:location -> core_type

(** Qualified identifier expression formed by extending an existing [Longident.t]. For
    static paths, prefer [Ppxlib.Ast_builder.Default.evar ~loc "A.B.c"]. *)
val qual_evar : loc:location -> Longident.t -> string list -> expression

(** Convert boxed numeric and character constants into their unboxed literal forms;
    non-constant expressions are returned unchanged. *)
val to_unboxed_constant_expr : loc:location -> expression -> expression

(** [(Stdlib.Obj.magic 0 : <field_type>)], a never-observed placeholder payload for an
    opaque value-layout field. *)
val opaque_default_payload_expr : loc:location -> core_type -> expression

(** True iff [expr] is a syntactic identifier reference matching one of [paths] (e.g.
    ["Float_u.nan"]). Used by NaN detection in float-kind sentinel overrides. *)
val expr_is_qualified_ident : loc:location -> expression -> string list -> bool

(** [@alloc a @ m = (heap @ global, stack @ local)] template attribute. *)
val alloc_heap_stack_attr : loc:location -> attribute

(** [@zero_alloc ignore] attribute. *)
val zero_alloc_ignore_attr : loc:location -> attribute

(** Attach [@exclave_if_stack a] to an expression. *)
val with_exclave_if_stack : loc:location -> expression -> expression

(** Attach [@alloc a] to an expression (typically a function identifier). *)
val with_alloc_var : loc:location -> expression -> expression

(** Wrap a structure item as [let%template ...]. *)
val pstr_template : loc:location -> structure_item -> structure_item

(** Build a
    [val%template name : arg_type @ m -> result_type @ m [\@\@alloc a \@ m = (heap \@ global, stack \@ local)]]
    signature item. *)
val templated_heap_stack_sig_value
  :  loc:location
  -> name:string
  -> arg_type:core_type
  -> result_type:core_type
  -> signature_item
