open Ppxlib
open Ast_builder.Default

let add_inline_zero_alloc ~loc expr =
  { expr with
    pexp_attributes =
      (fun ~loc name -> attribute ~loc ~name:{ txt = name; loc } ~payload:(PStr []))
        ~loc
        "inline"
      :: (fun ~loc name -> attribute ~loc ~name:{ txt = name; loc } ~payload:(PStr []))
           ~loc
           "zero_alloc"
      :: expr.pexp_attributes
  }
;;

let mk_val_binding ~loc name body =
  let pat = ppat_var ~loc { txt = name; loc } in
  let expr = add_inline_zero_alloc ~loc body in
  value_binding ~loc ~pat ~expr
;;

let fun_one ~loc arg_name body =
  pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = arg_name; loc }) body
;;

let fun_t_default ~loc body =
  pexp_fun
    ~loc
    Nolabel
    None
    (ppat_var ~loc { txt = "t"; loc })
    (pexp_fun
       ~loc
       (Labelled "default")
       None
       (ppat_var ~loc { txt = "default"; loc })
       body)
;;

let evar ~loc s = pexp_ident ~loc { txt = Lident s; loc }
let eapply ~loc f args = pexp_apply ~loc f (List.map (fun a -> Nolabel, a) args)
let enot ~loc e = eapply ~loc (evar ~loc "not") [ e ]
let tagged_option_type ~loc = [%type: #(bool * value)]
let tagged_none_expr ~loc payload = [%expr #(false, [%e payload])]
let tagged_some_expr ~loc payload = [%expr #(true, [%e payload])]

let eqident ~loc parts =
  let lid =
    match parts with
    | [] -> assert false
    | [ x ] -> Lident x
    | x :: rest -> List.fold_left (fun acc p -> Ldot (acc, p)) (Lident x) rest
  in
  pexp_ident ~loc { txt = lid; loc }
;;

let eqident_lid ~loc base parts =
  let lid = List.fold_left (fun acc p -> Ldot (acc, p)) base parts in
  pexp_ident ~loc { txt = lid; loc }
;;

let to_unboxed_constant_expr ~loc expr =
  let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
  match desc with
  | Pexp_constant c ->
    let c = Ppxlib_jane.Shim.Constant.of_parsetree c in
    (match c with
     | Pconst_integer (s, Some suffix) ->
       let uc =
         Ppxlib_jane.Shim.Constant.to_parsetree (Pconst_unboxed_integer (s, suffix))
       in
       pexp_constant ~loc uc
     | Pconst_float (s, suffix) ->
       let uc =
         Ppxlib_jane.Shim.Constant.to_parsetree (Pconst_unboxed_float (s, suffix))
       in
       pexp_constant ~loc uc
     | Pconst_char c ->
       let uc = Ppxlib_jane.Shim.Constant.to_parsetree (Pconst_untagged_char c) in
       pexp_constant ~loc uc
     | Pconst_unboxed_integer _ | Pconst_unboxed_float _ -> expr
     | Pconst_untagged_char _ -> expr
     | _ -> expr)
  | _ -> expr
;;

let primitive_sig ~loc name ty prim =
  pstr_primitive
    ~loc
    (value_description ~loc ~name:{ txt = name; loc } ~type_:ty ~prim:[ prim ])
;;

let attr_with_expr ~loc name expr =
  attribute ~loc ~name:{ txt = name; loc } ~payload:(PStr [ pstr_eval ~loc expr [] ])
;;

let alloc_heap_stack_attr ~loc =
  attr_with_expr ~loc "alloc" [%expr a @ m = (heap @ global, stack @ local)]
;;

let zero_alloc_ignore_attr ~loc = attr_with_expr ~loc "zero_alloc" [%expr ignore]
let alloc_var_attr ~loc = attr_with_expr ~loc "alloc" [%expr a]
let exclave_if_stack_attr ~loc = attr_with_expr ~loc "exclave_if_stack" [%expr a]
let with_attr attr expr = { expr with pexp_attributes = attr :: expr.pexp_attributes }
let with_exclave_if_stack ~loc expr = with_attr (exclave_if_stack_attr ~loc) expr
let with_alloc_var ~loc expr = with_attr (alloc_var_attr ~loc) expr

let pstr_template ~loc item =
  pstr_extension ~loc ({ txt = "template"; loc }, PStr [ item ]) []
;;

let psig_template ~loc item =
  psig_extension
    ~loc
    ( { txt = "template"; loc }
    , PSig (Ppxlib_jane.Ast_builder.Default.signature ~loc [ item ]) )
    []
;;

let mode_m ~loc : Ppxlib_jane.Shim.Modes.t =
  [ { txt = Ppxlib_jane.Shim.Mode.Mode "m"; loc } ]
;;

let templated_heap_stack_sig_value ~loc ~name ~arg_type ~result_type =
  let modes = mode_m ~loc in
  let ty =
    Ppxlib_jane.Ast_builder.Default.ptyp_arrow
      ~loc
      { arg_label = Nolabel; arg_modes = []; arg_type }
      { result_modes = modes; result_type }
  in
  let vd = value_description ~loc ~name:{ txt = name; loc } ~type_:ty ~prim:[] in
  let vd =
    { vd with
      pval_attributes = [ alloc_heap_stack_attr ~loc; zero_alloc_ignore_attr ~loc ]
    }
  in
  psig_template ~loc (psig_value ~loc vd)
;;

let rec string_of_longident = function
  | Lident s -> s
  | Ldot (lid, s) -> string_of_longident lid ^ "." ^ s
  | Lapply _ -> assert false
;;

let parse_bool_literal ~loc ~field_name expr =
  let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
  match desc with
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> true
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> false
  | _ -> Location.raise_errorf ~loc "ppx_uopt: %s must be a boolean literal" field_name
;;
