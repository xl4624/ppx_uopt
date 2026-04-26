(** Small AST construction helpers used across the generators.

    These functions keep the code generators focused on representation logic rather than
    repetitive [Ppxlib] boilerplate. *)

open Ppxlib

(** Attach [@inline] and [@zero_alloc] to a generated expression. *)
val add_inline_zero_alloc : loc:location -> expression -> expression

(** Build a value binding named [name], automatically marking the body [@inline] and
    [@zero_alloc]. *)
val mk_val_binding : loc:location -> string -> expression -> value_binding

(** [fun_one ~arg_name body] builds [fun arg_name -> body]. *)
val fun_one : loc:location -> string -> expression -> expression

(** [fun_t_default body] builds [fun t ~default -> body]. *)
val fun_t_default : loc:location -> expression -> expression

(** Identifier expression for a local variable. *)
val evar : loc:location -> string -> expression

(** Apply [f] to unlabeled arguments [args]. *)
val eapply : loc:location -> expression -> expression list -> expression

(** Negate a boolean expression with [not]. *)
val enot : loc:location -> expression -> expression

(** The tagged representation type [#(bool * value)]. *)
val tagged_option_type : loc:location -> core_type

(** Tagged [none] expression [#(false, payload)]. *)
val tagged_none_expr : loc:location -> expression -> expression

(** Tagged [some] expression [#(true, payload)]. *)
val tagged_some_expr : loc:location -> expression -> expression

(** Qualified identifier expression from path components such as [["Float_u"; "nan"]]. *)
val eqident : loc:location -> string list -> expression

(** Qualified identifier expression formed by extending an existing [Longident.t]. *)
val eqident_lid : loc:location -> Longident.t -> string list -> expression

(** Convert boxed numeric and character constants into their unboxed literal forms when
    possible. Non-constant expressions are returned unchanged. *)
val to_unboxed_constant_expr : loc:location -> expression -> expression

(** Primitive declaration helper used for generated support bindings. *)
val primitive_sig : loc:location -> string -> core_type -> string -> structure_item

(** Render a [Longident.t] for diagnostics. *)
val string_of_longident : Longident.t -> string

(** Parse a boolean literal field in the deriving payload.

    Raises a location error unless the expression is exactly [true] or [false]. *)
val parse_bool_literal : loc:location -> field_name:string -> expression -> bool

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
