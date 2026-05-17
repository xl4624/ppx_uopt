open Ppxlib
open Ast_builder.Default

let bare_attr ~loc name = attribute ~loc ~name:(Located.mk ~loc name) ~payload:(PStr [])

let attr_with_expr ~loc name expr =
  attribute ~loc ~name:(Located.mk ~loc name) ~payload:(PStr [ pstr_eval ~loc expr [] ])
;;

let with_attrs attrs expr = { expr with pexp_attributes = attrs @ expr.pexp_attributes }

(* The [@inline][@zero_alloc] attribute pair attached to every generated [Option] binding.
   When [is_none] transitively calls [caml_equal] (the only case being a sentinel-mode
   override on an opaque record field), the static checker can't prove zero-alloc, so we
   emit [@zero_alloc assume] on that whole transitive cone instead. *)
let inline_zero_alloc_attrs ~loc ~assume =
  [ bare_attr ~loc "inline"
  ; (if assume
     then attr_with_expr ~loc "zero_alloc" [%expr assume]
     else bare_attr ~loc "zero_alloc")
  ]
;;

let mk_val_binding ?(assume_zero_alloc = false) ~loc name body =
  let body = with_attrs (inline_zero_alloc_attrs ~loc ~assume:assume_zero_alloc) body in
  value_binding ~loc ~pat:(pvar ~loc name) ~expr:body
;;

(* Emit a single top-level non-recursive [let]. [let_def] binds [name = body] with no
   attributes; [let_inline] additionally marks the body [@inline][@zero_alloc] (or
   [assume] when [assume_zero_alloc] is true). [let_def_pat] takes an arbitrary pattern,
   used for bindings that need a type ascription (e.g. constrained contract payloads). *)
let let_def ~loc name body =
  pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar ~loc name) ~expr:body ]
;;

let let_def_pat ~loc pat body =
  pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:body ]
;;

let let_inline ?(assume_zero_alloc = false) ~loc name body =
  pstr_value ~loc Nonrecursive [ mk_val_binding ~assume_zero_alloc ~loc name body ]
;;

let tagged_option_type ~loc = [%type: #(bool * value)]
let tagged_none_expr ~loc payload = [%expr #(false, [%e payload])]
let tagged_some_expr ~loc payload = [%expr #(true, [%e payload])]

let qual_evar ~loc base parts =
  let lid = List.fold_left (fun acc p -> Ldot (acc, p)) base parts in
  pexp_ident ~loc (Located.mk ~loc lid)
;;

let to_unboxed_constant_expr ~loc expr =
  let mk c = pexp_constant ~loc (Ppxlib_jane.Shim.Constant.to_parsetree c) in
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
  | Pexp_constant c ->
    (match Ppxlib_jane.Shim.Constant.of_parsetree c with
     | Pconst_integer (s, Some suffix) -> mk (Pconst_unboxed_integer (s, suffix))
     | Pconst_float (s, suffix) -> mk (Pconst_unboxed_float (s, suffix))
     | Pconst_char c -> mk (Pconst_untagged_char c)
     | Pconst_integer (_, None)
     | Pconst_string _
     | Pconst_unboxed_integer _
     | Pconst_unboxed_float _
     | Pconst_untagged_char _ -> expr)
  | _ -> expr
;;

(* For an opaque value-layout field with no usable default, emit
   [(Stdlib.Obj.magic 0 : <field_type>)]. Used as a placeholder in the [none] constant for
   fields that the user did not list in [none = #{ ... }]: those fields are never
   inspected by [is_none] under partial-override semantics, so any well-typed value works.
   The field's type must be value-layout for the cast to type-check. *)
let opaque_default_payload_expr ~loc field_type =
  [%expr (Stdlib.Obj.magic 0 : [%t field_type])]
;;

let alloc_heap_stack_attr ~loc =
  attr_with_expr ~loc "alloc" [%expr a @ m = (heap @ global, stack @ local)]
;;

let zero_alloc_ignore_attr ~loc = attr_with_expr ~loc "zero_alloc" [%expr ignore]
let alloc_var_attr ~loc = attr_with_expr ~loc "alloc" [%expr a]
let exclave_if_stack_attr ~loc = attr_with_expr ~loc "exclave_if_stack" [%expr a]
let with_exclave_if_stack ~loc expr = with_attrs [ exclave_if_stack_attr ~loc ] expr
let with_alloc_var ~loc expr = with_attrs [ alloc_var_attr ~loc ] expr

let pstr_template ~loc item =
  pstr_extension ~loc (Located.mk ~loc "template", PStr [ item ]) []
;;

let psig_template ~loc item =
  let sig_ = Ppxlib_jane.Ast_builder.Default.signature ~loc [ item ] in
  psig_extension ~loc (Located.mk ~loc "template", PSig sig_) []
;;

let templated_heap_stack_sig_value ~loc ~name ~arg_type ~result_type =
  let ty = [%type: [%t arg_type] -> [%t result_type] @ m] in
  let vd =
    { (value_description ~loc ~name:(Located.mk ~loc name) ~type_:ty ~prim:[]) with
      pval_attributes = [ alloc_heap_stack_attr ~loc; zero_alloc_ignore_attr ~loc ]
    }
  in
  psig_template ~loc (psig_value ~loc vd)
;;

(* True iff [expr] is a syntactic identifier reference matching one of [paths]. Used by
   the NaN-detection paths, where we want to recognise canonical [Float_u.nan] /
   [Float.nan] etc. but not arbitrary expressions that happen to evaluate to NaN.
   [Longident.flatten_exn] raises on [Lapply], which can't appear in a [Pexp_ident]. *)
let expr_is_qualified_ident ~loc expr paths =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
  | Pexp_ident { txt; _ } ->
    List.mem (String.concat "." (Longident.flatten_exn txt)) paths
  | _ -> false
;;
