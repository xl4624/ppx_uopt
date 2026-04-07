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
