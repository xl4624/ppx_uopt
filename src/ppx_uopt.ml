open Ppxlib
open Ast_builder.Default

(* Create a located longident from a string *)
let lident ~loc s = { txt = Lident s; loc }

(* Create an attribute *)
let attr ~loc name = attribute ~loc ~name:{ txt = name; loc } ~payload:(PStr [])

(* Add [@inline] [@zero_alloc] attributes to an expression *)
let add_inline_zero_alloc ~loc expr =
  { expr with
    pexp_attributes = attr ~loc "inline" :: attr ~loc "zero_alloc" :: expr.pexp_attributes
  }
;;

(* Wrap a function body with [@inline] [@zero_alloc] as let binding *)
let mk_val_binding ~loc name body =
  let pat = ppat_var ~loc { txt = name; loc } in
  let expr = add_inline_zero_alloc ~loc body in
  value_binding ~loc ~pat ~expr
;;

(* [fun v -> body] *)
let fun_one ~loc arg_name body =
  pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = arg_name; loc }) body
;;

(* [fun t ~default -> body] *)
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

(* Reference an identifier *)
let evar ~loc s = pexp_ident ~loc (lident ~loc s)

(* Apply a function to arguments *)
let eapply ~loc f args = pexp_apply ~loc f (List.map (fun a -> Nolabel, a) args)

(* [not e] *)
let enot ~loc e = eapply ~loc (evar ~loc "not") [ e ]

(* Qualified identifier [M.x] from string list *)
let eqident ~loc parts =
  let lid =
    match parts with
    | [] -> assert false
    | [ x ] -> Lident x
    | x :: rest -> List.fold_left (fun acc p -> Ldot (acc, p)) (Lident x) rest
  in
  pexp_ident ~loc { txt = lid; loc }
;;

(* Qualified identifier from a Longident.t base *)
let eqident_lid ~loc base parts =
  let lid = List.fold_left (fun acc p -> Ldot (acc, p)) base parts in
  pexp_ident ~loc { txt = lid; loc }
;;

(* Convert a boxed constant expression to its unboxed equivalent.
   Users write { none = 0L } since #0L cannot be parsed in attribute payloads. *)
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

type scalar_kind =
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar
  | Char_u_scalar

let compare_scalar_kind a b = Stdlib.compare a b

let scalar_kind_of_core_type (ct : core_type) =
  let desc = Ppxlib_jane.Shim.Core_type_desc.of_parsetree ct.ptyp_desc in
  match desc with
  | Ptyp_constr ({ txt = Lident "float#"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Float_u", "t"); _ }, []) -> Some Float_u_scalar
  | Ptyp_constr ({ txt = Lident "float32#"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Float32_u", "t"); _ }, []) -> Some Float32_u_scalar
  | Ptyp_constr ({ txt = Lident "int32#"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Int32_u", "t"); _ }, []) -> Some Int32_u_scalar
  | Ptyp_constr ({ txt = Lident "int64#"; _ }, [])
  | Ptyp_constr ({ txt = Lident "bits64"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Int64_u", "t"); _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Bits64", "t"); _ }, []) -> Some Int64_u_scalar
  | Ptyp_constr ({ txt = Lident "nativeint#"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "Nativeint_u", "t"); _ }, []) ->
    Some Nativeint_u_scalar
  | Ptyp_constr ({ txt = Lident "int8#"; _ }, []) -> Some Int8_u_scalar
  | Ptyp_constr ({ txt = Lident "int16#"; _ }, []) -> Some Int16_u_scalar
  | Ptyp_constr ({ txt = Lident "int#"; _ }, []) -> Some Int_u_scalar
  | Ptyp_constr ({ txt = Lident "char#"; _ }, []) -> Some Char_u_scalar
  | _ -> None
;;

let scalar_kind_name = function
  | Float_u_scalar -> "float#"
  | Float32_u_scalar -> "float32#"
  | Int32_u_scalar -> "int32#"
  | Int64_u_scalar -> "int64#"
  | Nativeint_u_scalar -> "nativeint#"
  | Int8_u_scalar -> "int8#"
  | Int16_u_scalar -> "int16#"
  | Int_u_scalar -> "int#"
  | Char_u_scalar -> "char#"
;;

let scalar_requires_none_override = function
  | Float_u_scalar | Float32_u_scalar -> false
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar
  | Char_u_scalar -> true
;;

let scalar_default_none_expr ~loc = function
  | Float_u_scalar ->
    Some
      (eapply
         ~loc
         (eqident ~loc [ "Float_u"; "nan" ])
         [ pexp_construct ~loc (lident ~loc "()") None ])
  | Float32_u_scalar ->
    Some
      (eapply
         ~loc
         (eqident ~loc [ "Float32_u"; "nan" ])
         [ pexp_construct ~loc (lident ~loc "()") None ])
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar
  | Char_u_scalar -> None
;;

let scalar_none_override_expr ~loc kind expr =
  match kind with
  | Int_u_scalar ->
    let expr = to_unboxed_constant_expr ~loc expr in
    let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
    (match desc with
     | Pexp_constant c ->
       (match Ppxlib_jane.Shim.Constant.of_parsetree c with
        | Pconst_integer (_, None) -> eapply ~loc (evar ~loc "_uopt_of_int") [ expr ]
        | _ ->
          Location.raise_errorf
            ~loc
            "ppx_uopt: int# none sentinel must be an int literal, e.g. \
             [@@deriving unboxed_option { none = 0 }]")
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: none sentinel must be a constant literal")
  | _ -> to_unboxed_constant_expr ~loc expr
;;

let scalar_none_expr ~loc kind ~none_override =
  match none_override with
  | Some expr -> scalar_none_override_expr ~loc kind expr
  | None ->
    (match scalar_default_none_expr ~loc kind with
     | Some expr -> expr
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: %s requires a none sentinel, e.g. [@@deriving unboxed_option \
          { none = ... }]"
         (scalar_kind_name kind))
;;

let scalar_equal_fn ~loc = function
  | Float_u_scalar -> eqident ~loc [ "Float_u"; "equal" ]
  | Float32_u_scalar -> eqident ~loc [ "Float32_u"; "equal" ]
  | Int32_u_scalar -> eqident ~loc [ "Int32_u"; "equal" ]
  | Int64_u_scalar -> eqident ~loc [ "Int64_u"; "equal" ]
  | Nativeint_u_scalar -> eqident ~loc [ "Nativeint_u"; "equal" ]
  | Int8_u_scalar -> evar ~loc "_uopt_equal_int8"
  | Int16_u_scalar -> evar ~loc "_uopt_equal_int16"
  | Int_u_scalar -> evar ~loc "_uopt_equal_int"
  | Char_u_scalar -> evar ~loc "_uopt_equal_char"
;;

let scalar_is_none_body ~loc kind ~none_override t_expr =
  match kind, none_override with
  | Float_u_scalar, None -> eapply ~loc (eqident ~loc [ "Float_u"; "is_nan" ]) [ t_expr ]
  | Float32_u_scalar, None ->
    eapply ~loc (eqident ~loc [ "Float32_u"; "is_nan" ]) [ t_expr ]
  | _ ->
    let sentinel = scalar_none_expr ~loc kind ~none_override in
    eapply ~loc (scalar_equal_fn ~loc kind) [ t_expr; sentinel ]
;;

let primitive_sig ~loc name ty prim =
  pstr_primitive
    ~loc
    (value_description ~loc ~name:{ txt = name; loc } ~type_:ty ~prim:[ prim ])
;;

let scalar_helper_items ~loc = function
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar -> []
  | Int8_u_scalar ->
    [ primitive_sig
        ~loc
        "_uopt_equal_int8"
        (ptyp_arrow
           ~loc
           Nolabel
           (ptyp_constr ~loc (lident ~loc "int8#") [])
           (ptyp_arrow
              ~loc
              Nolabel
              (ptyp_constr ~loc (lident ~loc "int8#") [])
              [%type: bool]))
        "%int8#_equal"
    ]
  | Int16_u_scalar ->
    [ primitive_sig
        ~loc
        "_uopt_equal_int16"
        (ptyp_arrow
           ~loc
           Nolabel
           (ptyp_constr ~loc (lident ~loc "int16#") [])
           (ptyp_arrow
              ~loc
              Nolabel
              (ptyp_constr ~loc (lident ~loc "int16#") [])
              [%type: bool]))
        "%int16#_equal"
    ]
  | Int_u_scalar ->
    [ primitive_sig
        ~loc
        "_uopt_of_int"
        (ptyp_arrow ~loc Nolabel [%type: int] (ptyp_constr ~loc (lident ~loc "int#") []))
        "%int#_of_int"
    ; primitive_sig
        ~loc
        "_uopt_equal_int"
        (ptyp_arrow
           ~loc
           Nolabel
           (ptyp_constr ~loc (lident ~loc "int#") [])
           (ptyp_arrow
              ~loc
              Nolabel
              (ptyp_constr ~loc (lident ~loc "int#") [])
              [%type: bool]))
        "%int#_equal"
    ]
  | Char_u_scalar ->
    [ primitive_sig
        ~loc
        "_uopt_equal_int8"
        (ptyp_arrow
           ~loc
           Nolabel
           (ptyp_constr ~loc (lident ~loc "int8#") [])
           (ptyp_arrow
              ~loc
              Nolabel
              (ptyp_constr ~loc (lident ~loc "int8#") [])
              [%type: bool]))
        "%int8#_equal"
    ; primitive_sig
        ~loc
        "_uopt_char_to_int8"
        (ptyp_arrow
           ~loc
           Nolabel
           (ptyp_constr ~loc (lident ~loc "char#") [])
           (ptyp_constr ~loc (lident ~loc "int8#") []))
        "%identity"
    ; pstr_value
        ~loc
        Nonrecursive
        [ mk_val_binding
            ~loc
            "_uopt_equal_char"
            (fun_one
               ~loc
               "x"
               (fun_one
                  ~loc
                  "y"
                  (eapply
                     ~loc
                     (evar ~loc "_uopt_equal_int8")
                     [ eapply ~loc (evar ~loc "_uopt_char_to_int8") [ evar ~loc "x" ]
                     ; eapply ~loc (evar ~loc "_uopt_char_to_int8") [ evar ~loc "y" ]
                     ])))
        ]
    ]
;;

type record_field_kind =
  | Record_field_scalar of scalar_kind

let classify_record_field ~loc (ld : label_declaration) =
  let field_name = ld.pld_name.txt in
  match scalar_kind_of_core_type ld.pld_type with
  | Some scalar_kind -> Record_field_scalar scalar_kind
  | None ->
    let ct_desc = Ppxlib_jane.Shim.Core_type_desc.of_parsetree ld.pld_type.ptyp_desc in
    (match ct_desc with
     | Ptyp_constr ({ txt = Ldot (_, "t"); _ }, []) ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unsupported field type for field '%s'. Module-qualified field types \
          are not assumed to have an Option module; only supported unboxed scalar fields \
          may appear in unboxed-record options."
         field_name
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unsupported field type for field '%s'. Only supported unboxed scalar \
          fields may appear in unboxed-record options."
         field_name)
;;

let record_scalar_kinds ~loc labels =
  labels
  |> List.map (fun ld ->
    match classify_record_field ~loc ld with
    | Record_field_scalar kind -> kind)
  |> List.sort_uniq compare_scalar_kind
;;

type type_info =
  | Scalar of scalar_kind
  | Unboxed_record of label_declaration list

let detect_type_info ~loc (td : type_declaration) ~none_override =
  let kind = Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind in
  match kind with
  | Ptype_record_unboxed_product labels -> Unboxed_record labels
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | Some ct ->
       (match scalar_kind_of_core_type ct with
        | Some scalar_kind ->
          if scalar_requires_none_override scalar_kind && Option.is_none none_override
          then
            Location.raise_errorf
              ~loc
              "ppx_uopt: %s requires a none sentinel, e.g. [@@deriving unboxed_option \
               { none = ... }]"
              (scalar_kind_name scalar_kind);
          Scalar scalar_kind
        | None ->
          Location.raise_errorf
            ~loc
            "ppx_uopt: unsupported type. Supported scalar types are float#, \
             float32#, int32#, int64#/bits64, nativeint#, int8#, int16#, int#, \
             char#, and unboxed records.")
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: abstract type with no manifest is not supported")
  | _ ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: only unboxed record products and unboxed scalar types are supported"
;;

let gen_unboxed_record_none ~loc labels ~none_override =
  match none_override with
  | Some expr ->
    let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
    (match desc with
     | Pexp_record_unboxed_product _ -> expr
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unboxed-record none sentinel must be an unboxed record literal, e.g. \
          #{ x = #0s }")
  | None ->
    let fields =
      List.map
        (fun (ld : label_declaration) ->
          let field_name = ld.pld_name.txt in
          let field_lid = lident ~loc field_name in
          let field_none =
            match classify_record_field ~loc ld with
            | Record_field_scalar kind ->
              (match scalar_default_none_expr ~loc kind with
               | Some expr -> expr
               | None ->
                 Location.raise_errorf
                   ~loc
                   "ppx_uopt: unboxed-record field '%s' of type %s requires an explicit \
                    none sentinel for the whole record"
                   field_name
                   (scalar_kind_name kind))
          in
          field_lid, field_none)
        labels
    in
    pexp_record_unboxed_product ~loc fields None
;;

(* Generate is_none check for unboxed record by OR-ing across all sentinel-bearing fields *)
let gen_unboxed_record_is_none ~loc labels ~none_override ~none_expr t_expr =
  let checks =
    List.map
      (fun (ld : label_declaration) ->
        let field_name = ld.pld_name.txt in
        let field_lid = lident ~loc field_name in
        match classify_record_field ~loc ld with
        | Record_field_scalar kind ->
          let field_access = pexp_unboxed_field ~loc t_expr (lident ~loc field_name) in
          let field_none_override =
            Option.map (fun _ -> pexp_unboxed_field ~loc none_expr field_lid) none_override
          in
          scalar_is_none_body ~loc kind ~none_override:field_none_override field_access)
      labels
  in
  match checks with
  | [] ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: cannot generate is_none for unboxed record with no checkable fields"
  | first :: rest ->
    List.fold_left (fun acc check -> [%expr [%e acc] || [%e check]]) first rest
;;

(* Generate the structure (implementation) for the Option module *)
let gen_str_option ~loc ~type_name ~type_info ~none_override ~is_none_override =
  let type_value =
    pstr_type
      ~loc
      Nonrecursive
      [ type_declaration
          ~loc
          ~name:{ txt = "value"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest:(Some (ptyp_constr ~loc (lident ~loc type_name) []))
      ]
  in
  let type_t =
    pstr_type
      ~loc
      Nonrecursive
      [ type_declaration
          ~loc
          ~name:{ txt = "t"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest:(Some (ptyp_constr ~loc (lident ~loc "value") []))
      ]
  in
  let none_expr =
    match type_info with
    | Scalar kind -> scalar_none_expr ~loc kind ~none_override
    | Unboxed_record labels -> gen_unboxed_record_none ~loc labels ~none_override
  in
  let helper_items =
    match type_info with
    | Scalar kind -> scalar_helper_items ~loc kind
    | Unboxed_record labels ->
      record_scalar_kinds ~loc labels |> List.concat_map (scalar_helper_items ~loc)
  in
  let is_none_body t_expr =
    match is_none_override with
    | Some is_none_fn -> eapply ~loc is_none_fn [ t_expr ]
    | None ->
      (match type_info with
       | Scalar kind -> scalar_is_none_body ~loc kind ~none_override t_expr
       | Unboxed_record labels ->
         gen_unboxed_record_is_none ~loc labels ~none_override ~none_expr t_expr)
  in
  let let_none = pstr_value ~loc Nonrecursive [ mk_val_binding ~loc "none" none_expr ] in
  let let_some =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding ~loc "some" (fun_one ~loc "v" (evar ~loc "v")) ]
  in
  let let_is_none =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding ~loc "is_none" (fun_one ~loc "t" (is_none_body (evar ~loc "t"))) ]
  in
  let let_is_some =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "is_some"
          (fun_one
             ~loc
             "t"
             (enot ~loc (eapply ~loc (evar ~loc "is_none") [ evar ~loc "t" ])))
      ]
  in
  let let_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value"
          (fun_t_default ~loc [%expr if is_none t then default else t])
      ]
  in
  let msg = estring ~loc (Printf.sprintf "%s.Option.value_exn: none" type_name) in
  (* Build: match Stdlib.raise (Failure msg) with (_ : _uopt_empty) -> .
     where _uopt_empty is a locally-defined empty variant type.
     This is necessary for layout-polymorphic unreachable code. *)
  let nothing_ty = ptyp_constr ~loc (lident ~loc "_uopt_empty") [] in
  let raise_match =
    pexp_match
      ~loc
      [%expr Stdlib.raise (Failure [%e msg])]
      [ { pc_lhs = ppat_constraint ~loc (ppat_var ~loc { txt = "_"; loc }) nothing_ty
        ; pc_guard = None
        ; pc_rhs = pexp_unreachable ~loc
        }
      ]
  in
  let let_value_exn =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value_exn"
          (fun_one ~loc "t" [%expr if is_none t then [%e raise_match] else t])
      ]
  in
  let let_unchecked_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding ~loc "unchecked_value" (fun_one ~loc "t" (evar ~loc "t")) ]
  in
  let optional_syntax_mod =
    pstr_module
      ~loc
      (module_binding
         ~loc
         ~name:{ txt = Some "Optional_syntax"; loc }
         ~expr:
           (pmod_structure
              ~loc
              [ pstr_module
                  ~loc
                  (module_binding
                     ~loc
                     ~name:{ txt = Some "Optional_syntax"; loc }
                     ~expr:
                       (pmod_structure
                          ~loc
                          [ pstr_value
                              ~loc
                              Nonrecursive
                              [ value_binding
                                  ~loc
                                  ~pat:(ppat_var ~loc { txt = "is_none"; loc })
                                  ~expr:
                                    (add_inline_zero_alloc
                                       ~loc
                                       (fun_one
                                          ~loc
                                          "t"
                                          (eapply
                                             ~loc
                                             (evar ~loc "is_none")
                                             [ evar ~loc "t" ])))
                              ]
                          ; pstr_value
                              ~loc
                              Nonrecursive
                              [ value_binding
                                  ~loc
                                  ~pat:(ppat_var ~loc { txt = "unsafe_value"; loc })
                                  ~expr:
                                    (add_inline_zero_alloc
                                       ~loc
                                       (fun_one
                                          ~loc
                                          "t"
                                          (eapply
                                             ~loc
                                             (evar ~loc "unchecked_value")
                                             [ evar ~loc "t" ])))
                              ]
                          ]))
              ]))
  in
  let type_empty =
    pstr_type
      ~loc
      Nonrecursive
      [ type_declaration
          ~loc
          ~name:{ txt = "_uopt_empty"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:(Ptype_variant [])
          ~private_:Public
          ~manifest:None
      ]
  in
  let suppress_warnings =
    pstr_attribute
      ~loc
      (attribute
         ~loc
         ~name:{ txt = "ocaml.warning"; loc }
         ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-34-60") [] ]))
  in
  [ suppress_warnings; type_value; type_t ]
  @ helper_items
  @ [ let_none
    ; let_some
    ; let_is_none
    ; let_is_some
    ; let_value
    ; type_empty
    ; let_value_exn
    ; let_unchecked_value
    ; optional_syntax_mod
    ]
;;

(* Generate the signature for the Option module *)
let gen_sig_option ~loc ~type_name =
  let t_typ = ptyp_constr ~loc (lident ~loc type_name) [] in
  let value_typ = ptyp_constr ~loc (lident ~loc "value") [] in
  let bool_typ = [%type: bool] in
  let mk_val name ty =
    psig_value ~loc (value_description ~loc ~name:{ txt = name; loc } ~type_:ty ~prim:[])
  in
  [ psig_type
      ~loc
      Nonrecursive
      [ type_declaration
          ~loc
          ~name:{ txt = "value"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest:(Some t_typ)
      ]
  ; psig_type
      ~loc
      Nonrecursive
      [ type_declaration
          ~loc
          ~name:{ txt = "t"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest:(Some (ptyp_constr ~loc (lident ~loc "value") []))
      ]
  ; mk_val "none" value_typ
  ; mk_val "some" (ptyp_arrow ~loc Nolabel value_typ value_typ)
  ; mk_val "is_none" (ptyp_arrow ~loc Nolabel value_typ bool_typ)
  ; mk_val "is_some" (ptyp_arrow ~loc Nolabel value_typ bool_typ)
  ; mk_val
      "value"
      (ptyp_arrow
         ~loc
         Nolabel
         value_typ
         (ptyp_arrow ~loc (Labelled "default") value_typ value_typ))
  ; mk_val "value_exn" (ptyp_arrow ~loc Nolabel value_typ value_typ)
  ; mk_val "unchecked_value" (ptyp_arrow ~loc Nolabel value_typ value_typ)
  ; psig_module
      ~loc
      (module_declaration
         ~loc
         ~name:{ txt = Some "Optional_syntax"; loc }
         ~type_:
           (pmty_signature
              ~loc
              [ psig_module
                  ~loc
                  (module_declaration
                     ~loc
                     ~name:{ txt = Some "Optional_syntax"; loc }
                     ~type_:
                       (pmty_signature
                          ~loc
                          [ mk_val "is_none" (ptyp_arrow ~loc Nolabel value_typ bool_typ)
                          ; mk_val
                              "unsafe_value"
                              (ptyp_arrow ~loc Nolabel value_typ value_typ)
                          ]))
              ]))
  ]
;;

(* Wrap items in a module *)
let wrap_in_module_str ~loc name items =
  pstr_module
    ~loc
    (module_binding ~loc ~name:{ txt = Some name; loc } ~expr:(pmod_structure ~loc items))
;;

let wrap_in_module_sig ~loc name items =
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:{ txt = Some name; loc }
       ~type_:(pmty_signature ~loc items))
;;

(* The deriver args: { none = <expr>; is_none = <expr> } *)
let args =
  let open Deriving.Args in
  empty
  +> arg
       "none"
       Ast_pattern.(
         as__
           (pexp_constant drop
            ||| pexp_record_unboxed_product drop drop))
  +> arg "is_none" Ast_pattern.__
;;

let str_type_decl =
  Deriving.Generator.make args (fun ~loc ~path:_ (_rec_flag, tds) none_opt is_none_opt ->
    match tds with
    | [ td ] ->
      let type_name = td.ptype_name.txt in
      let type_info = detect_type_info ~loc td ~none_override:none_opt in
      let items =
        gen_str_option
          ~loc
          ~type_name
          ~type_info
          ~none_override:none_opt
          ~is_none_override:is_none_opt
      in
      [ wrap_in_module_str ~loc "Option" items ]
    | _ ->
      Location.raise_errorf ~loc "ppx_uopt: only single type declarations are supported")
;;

let sig_args =
  let open Deriving.Args in
  empty
  +> arg
       "none"
       Ast_pattern.(
         as__
           (pexp_constant drop
            ||| pexp_record_unboxed_product drop drop))
  +> arg "is_none" Ast_pattern.__
;;

let sig_type_decl =
  Deriving.Generator.make
    sig_args
    (fun ~loc ~path:_ (_rec_flag, tds) _none_opt _is_none_opt ->
       match tds with
       | [ td ] ->
         let type_name = td.ptype_name.txt in
         let items = gen_sig_option ~loc ~type_name in
         [ wrap_in_module_sig ~loc "Option" items ]
       | _ ->
         Location.raise_errorf
           ~loc
           "ppx_uopt: only single type declarations are supported")
;;

let () = Deriving.add "unboxed_option" ~str_type_decl ~sig_type_decl |> Deriving.ignore
