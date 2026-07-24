open Ppxlib
open Ast_builder.Default

let bare_attr ~loc name = attribute ~loc ~name:(Located.mk ~loc name) ~payload:(PStr [])

let attr_with_expr ~loc name expr =
  attribute ~loc ~name:(Located.mk ~loc name) ~payload:(PStr [ pstr_eval ~loc expr [] ])
;;

let with_attrs attrs expr = { expr with pexp_attributes = attrs @ expr.pexp_attributes }

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

let let_def_pat ~loc pat body =
  pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:body ]
;;

let let_def ~loc name body = let_def_pat ~loc (pvar ~loc name) body

let let_inline ?(assume_zero_alloc = false) ~loc name body =
  pstr_value ~loc Nonrecursive [ mk_val_binding ~assume_zero_alloc ~loc name body ]
;;

let tagged_option_type ~loc = [%type: #(bool * value)]

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

(* Requires [field_type] to have layout [value]; see
   [Classification.classify_record_field]. *)
let opaque_default_payload_expr ~loc field_type =
  [%expr (Stdlib.Obj.magic 0 : [%t field_type])]
;;

let alloc_heap_stack_attr ~loc =
  attr_with_expr ~loc "alloc" [%expr a @ m = (heap @ global, stack @ local)]
;;

let zero_alloc_ignore_attr ~loc = attr_with_expr ~loc "zero_alloc" [%expr ignore]

let with_exclave_if_stack ~loc expr =
  with_attrs [ attr_with_expr ~loc "exclave_if_stack" [%expr a] ] expr
;;

let with_alloc_var ~loc expr = with_attrs [ attr_with_expr ~loc "alloc" [%expr a] ] expr

let pstr_template ~loc item =
  pstr_extension ~loc (Located.mk ~loc "template", PStr [ item ]) []
;;

let templated_heap_stack_sig_value ~loc ~name ~arg_type ~result_type =
  let ty = [%type: [%t arg_type] -> [%t result_type] @ m] in
  let vd =
    { (value_description ~loc ~name:(Located.mk ~loc name) ~type_:ty ~prim:[]) with
      pval_attributes = [ alloc_heap_stack_attr ~loc; zero_alloc_ignore_attr ~loc ]
    }
  in
  let sig_ = Ppxlib_jane.Ast_builder.Default.signature ~loc [ psig_value ~loc vd ] in
  psig_extension ~loc (Located.mk ~loc "template", PSig sig_) []
;;

(* [Longident.flatten_exn] raises on [Lapply], which can't appear in a [Pexp_ident]. *)
let expr_is_qualified_ident ~loc expr paths =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
  | Pexp_ident { txt; _ } ->
    List.mem (String.concat "." (Longident.flatten_exn txt)) paths
  | _ -> false
;;
