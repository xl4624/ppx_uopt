open Ppxlib

(** Small AST construction helpers used across the generators.

    These functions keep the code generators focused on representation logic rather than
    repetitive [Ppxlib] boilerplate. *)

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
