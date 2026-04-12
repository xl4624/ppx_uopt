open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers
open Classification

let repr_kind_of_options ~none_override ~sentinel_override =
  if Option.is_some none_override || sentinel_override then Sentinel_repr else Tagged_repr
;;

let suppress_warnings ~loc =
  pstr_attribute
    ~loc
    (attribute
       ~loc
       ~name:{ txt = "ocaml.warning"; loc }
       ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-34-60") [] ]))
;;

let type_empty ~loc =
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
;;

let raise_match ~loc ~type_name =
  let msg = estring ~loc (Printf.sprintf "%s.Option.value_exn: none" type_name) in
  let nothing_ty = ptyp_constr ~loc { txt = Lident "_uopt_empty"; loc } [] in
  pexp_match
    ~loc
    [%expr Stdlib.raise (Failure [%e msg])]
    [ { pc_lhs = ppat_constraint ~loc (ppat_var ~loc { txt = "_"; loc }) nothing_ty
      ; pc_guard = None
      ; pc_rhs = pexp_unreachable ~loc
      }
    ]
;;

let optional_syntax_mod ~loc =
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
;;

let type_value_decl ~loc ~type_name =
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
        ~manifest:(Some (ptyp_constr ~loc { txt = Lident type_name; loc } []))
    ]
;;

let gen_sig_items ~loc ~type_name ~t_typ =
  let manifest_typ = ptyp_constr ~loc { txt = Lident type_name; loc } [] in
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
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
          ~manifest:(Some manifest_typ)
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
          ~manifest:(Some t_typ)
      ]
  ; mk_val "none" t_typ
  ; mk_val "some" (ptyp_arrow ~loc Nolabel value_typ t_typ)
  ; mk_val "is_none" (ptyp_arrow ~loc Nolabel t_typ bool_typ)
  ; mk_val "is_some" (ptyp_arrow ~loc Nolabel t_typ bool_typ)
  ; mk_val
      "value"
      (ptyp_arrow
         ~loc
         Nolabel
         t_typ
         (ptyp_arrow ~loc (Labelled "default") value_typ value_typ))
  ; mk_val "value_exn" (ptyp_arrow ~loc Nolabel t_typ value_typ)
  ; mk_val "unchecked_value" (ptyp_arrow ~loc Nolabel t_typ value_typ)
  ; mk_val
      "sexp_of_value"
      (ptyp_arrow
         ~loc
         Nolabel
         value_typ
         (ptyp_constr ~loc { txt = Ldot (Ldot (Lident "Sexplib0", "Sexp"), "t"); loc } []))
  ; mk_val
      "sexp_of_t"
      (ptyp_arrow
         ~loc
         Nolabel
         t_typ
         (ptyp_constr ~loc { txt = Ldot (Ldot (Lident "Sexplib0", "Sexp"), "t"); loc } []))
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
                          [ mk_val "is_none" (ptyp_arrow ~loc Nolabel t_typ bool_typ)
                          ; mk_val
                              "unsafe_value"
                              (ptyp_arrow ~loc Nolabel t_typ value_typ)
                          ]))
              ]))
  ]
;;

let default_payload_expr ~loc = function
  | Scalar kind -> Scalar_gen.default_payload_expr ~loc kind
  | Unboxed_record labels ->
    let fields =
      List.map
        (fun (ld : label_declaration) ->
          let field_name = ld.pld_name.txt in
          let field_lid = { txt = Lident field_name; loc } in
          let field_payload =
            match classify_record_field ~loc ld with
            | Record_field_scalar kind -> Scalar_gen.default_payload_expr ~loc kind
            | Record_field_contract _ ->
              evar ~loc (Record_gen.contract_payload_name field_name)
          in
          field_lid, field_payload)
        labels
    in
    pexp_record_unboxed_product ~loc fields None
;;

let gen_str_option
  ~loc
  ~type_name
  ~type_info
  ~none_override
  ~sentinel_override
  ~is_none_override
  =
  let repr_kind = repr_kind_of_options ~none_override ~sentinel_override in
  (match repr_kind, is_none_override with
   | Tagged_repr, Some _ ->
     Location.raise_errorf
       ~loc
       "ppx_uopt: custom is_none overrides require an explicit none = ... sentinel"
   | Sentinel_repr, _ | Tagged_repr, None -> ());
  let type_value = type_value_decl ~loc ~type_name in
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
          ~manifest:
            (Some
               (match repr_kind with
                | Sentinel_repr -> ptyp_constr ~loc { txt = Lident "value"; loc } []
                | Tagged_repr -> tagged_option_type ~loc))
      ]
  in
  let helper_items =
    match type_info with
    | Scalar kind -> Scalar_gen.helper_items ~loc kind
    | Unboxed_record labels ->
      let scalar_kinds =
        labels
        |> List.filter_map (fun ld ->
          match classify_record_field ~loc ld with
          | Record_field_scalar kind -> Some kind
          | Record_field_contract _ -> None)
        |> List.sort_uniq compare_scalar_kind
      in
      scalar_kinds |> List.concat_map (Scalar_gen.helper_items ~loc)
  in
  let contract_items =
    match type_info with
    | Scalar _ -> []
    | Unboxed_record labels ->
      Record_gen.contract_helper_items
        ~loc
        labels
        ~need_is_none:
          (match repr_kind with
           | Sentinel_repr -> true
           | Tagged_repr -> false)
  in
  let sentinel_none_expr =
    match repr_kind with
    | Sentinel_repr ->
      Some
        (match type_info with
         | Scalar kind -> Scalar_gen.none_expr ~loc ~kind ~none_override
         | Unboxed_record labels ->
           Record_gen.gen_unboxed_record_none ~loc labels ~none_override)
    | Tagged_repr -> None
  in
  let tagged_none_payload_expr =
    match repr_kind with
    | Sentinel_repr -> None
    | Tagged_repr -> Some (default_payload_expr ~loc type_info)
  in
  let let_none =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "none"; loc })
          ~expr:
            (match repr_kind with
             | Sentinel_repr -> Option.get sentinel_none_expr
             | Tagged_repr -> tagged_none_expr ~loc (Option.get tagged_none_payload_expr))
      ]
  in
  let let_some =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "some"
          (fun_one
             ~loc
             "v"
             (match repr_kind with
              | Sentinel_repr -> evar ~loc "v"
              | Tagged_repr -> tagged_some_expr ~loc (evar ~loc "v")))
      ]
  in
  let let_is_none =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "is_none"
          (fun_one
             ~loc
             "t"
             (match repr_kind with
              | Sentinel_repr ->
                (match is_none_override with
                 | Some is_none_fn -> eapply ~loc is_none_fn [ evar ~loc "t" ]
                 | None ->
                   (match type_info with
                    | Scalar kind ->
                      Scalar_gen.is_none_body ~loc ~kind ~none_override (evar ~loc "t")
                    | Unboxed_record labels ->
                      Record_gen.gen_unboxed_record_is_none_sentinel
                        ~loc
                        labels
                        ~none_override
                        (evar ~loc "t")))
              | Tagged_repr ->
                [%expr
                  match t with
                  | #(is_some, _) -> not is_some]))
      ]
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
             (match repr_kind with
              | Sentinel_repr ->
                enot ~loc (eapply ~loc (evar ~loc "is_none") [ evar ~loc "t" ])
              | Tagged_repr ->
                [%expr
                  match t with
                  | #(is_some, _) -> is_some]))
      ]
  in
  let let_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value"
          (fun_t_default
             ~loc
             (match repr_kind with
              | Sentinel_repr -> [%expr if is_none t then default else t]
              | Tagged_repr ->
                [%expr
                  match t with
                  | #(is_some, value) -> if is_some then value else default]))
      ]
  in
  let raise_match = raise_match ~loc ~type_name in
  let let_value_exn =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value_exn"
          (fun_one
             ~loc
             "t"
             (match repr_kind with
              | Sentinel_repr -> [%expr if is_none t then [%e raise_match] else t]
              | Tagged_repr ->
                [%expr
                  match t with
                  | #(is_some, value) -> if is_some then value else [%e raise_match]]))
      ]
  in
  let let_unchecked_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "unchecked_value"
          (fun_one
             ~loc
             "t"
             (match repr_kind with
              | Sentinel_repr -> evar ~loc "t"
              | Tagged_repr ->
                [%expr
                  match t with
                  | #(_, value) -> value]))
      ]
  in
  let sexp_of_value_body =
    match type_info with
    | Scalar kind -> Scalar_gen.sexp_of_value_expr ~loc kind (evar ~loc "v")
    | Unboxed_record labels ->
      let field_sexps =
        List.map
          (fun (ld : label_declaration) ->
            let field_name = ld.pld_name.txt in
            let field_access =
              pexp_unboxed_field ~loc (evar ~loc "v") { txt = Lident field_name; loc }
            in
            let field_sexp =
              match classify_record_field ~loc ld with
              | Record_field_scalar kind ->
                Scalar_gen.sexp_of_value_expr ~loc kind field_access
              | Record_field_contract base ->
                eapply
                  ~loc
                  (eqident_lid ~loc base [ "Option"; "sexp_of_value" ])
                  [ field_access ]
            in
            [%expr
              Sexplib0.Sexp.List
                [ Sexplib0.Sexp.Atom [%e estring ~loc field_name]; [%e field_sexp] ]])
          labels
      in
      let list_expr =
        List.fold_right
          (fun elem acc -> [%expr [%e elem] :: [%e acc]])
          field_sexps
          [%expr []]
      in
      [%expr Sexplib0.Sexp.List [%e list_expr]]
  in
  let let_sexp_of_value =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "sexp_of_value"; loc })
          ~expr:(fun_one ~loc "v" sexp_of_value_body)
      ]
  in
  let let_sexp_of_t =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "sexp_of_t"; loc })
          ~expr:
            (fun_one
               ~loc
               "t"
               [%expr
                 Sexplib0.Sexp_conv.sexp_of_option
                   (fun x -> x)
                   (if is_none t then None else Some (sexp_of_value (unchecked_value t)))])
      ]
  in
  [ suppress_warnings ~loc; type_value; type_t ]
  @ helper_items
  @ contract_items
  @ [ let_none
    ; let_some
    ; let_is_none
    ; let_is_some
    ; let_value
    ; type_empty ~loc
    ; let_value_exn
    ; let_unchecked_value
    ; let_sexp_of_value
    ; let_sexp_of_t
    ; optional_syntax_mod ~loc
    ]
;;

let gen_sig_option ~loc ~type_name ~none_override ~sentinel_override =
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
  let t_typ =
    match repr_kind_of_options ~none_override ~sentinel_override with
    | Sentinel_repr -> value_typ
    | Tagged_repr -> tagged_option_type ~loc
  in
  gen_sig_items ~loc ~type_name ~t_typ
;;

let gen_str_alias ~loc ~type_name ~base =
  let m_option parts = eqident_lid ~loc (Ldot (base, "Option")) parts in
  let type_value = type_value_decl ~loc ~type_name in
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
          ~manifest:
            (Some (ptyp_constr ~loc { txt = Ldot (Ldot (base, "Option"), "t"); loc } []))
      ]
  in
  let let_none =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "none"; loc })
          ~expr:(m_option [ "none" ])
      ]
  in
  let let_some =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "some"
          (fun_one ~loc "v" (eapply ~loc (m_option [ "some" ]) [ evar ~loc "v" ]))
      ]
  in
  let let_is_none =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "is_none"
          (fun_one ~loc "t" (eapply ~loc (m_option [ "is_none" ]) [ evar ~loc "t" ]))
      ]
  in
  let let_is_some =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "is_some"
          (fun_one ~loc "t" (eapply ~loc (m_option [ "is_some" ]) [ evar ~loc "t" ]))
      ]
  in
  let let_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value"
          (fun_t_default
             ~loc
             (pexp_apply
                ~loc
                (m_option [ "value" ])
                [ Nolabel, evar ~loc "t"; Labelled "default", evar ~loc "default" ]))
      ]
  in
  let raise_match = raise_match ~loc ~type_name in
  let let_value_exn =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "value_exn"
          (fun_one
             ~loc
             "t"
             [%expr if is_none t then [%e raise_match] else unchecked_value t])
      ]
  in
  let let_unchecked_value =
    pstr_value
      ~loc
      Nonrecursive
      [ mk_val_binding
          ~loc
          "unchecked_value"
          (fun_one
             ~loc
             "t"
             (eapply ~loc (m_option [ "unchecked_value" ]) [ evar ~loc "t" ]))
      ]
  in
  let let_sexp_of_value =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "sexp_of_value"; loc })
          ~expr:
            (fun_one
               ~loc
               "v"
               (eapply ~loc (m_option [ "sexp_of_value" ]) [ evar ~loc "v" ]))
      ]
  in
  let let_sexp_of_t =
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_var ~loc { txt = "sexp_of_t"; loc })
          ~expr:
            (fun_one ~loc "t" (eapply ~loc (m_option [ "sexp_of_t" ]) [ evar ~loc "t" ]))
      ]
  in
  [ suppress_warnings ~loc
  ; type_value
  ; type_t
  ; let_none
  ; let_some
  ; let_is_none
  ; let_is_some
  ; let_value
  ; let_unchecked_value
  ; type_empty ~loc
  ; let_value_exn
  ; let_sexp_of_value
  ; let_sexp_of_t
  ; optional_syntax_mod ~loc
  ]
;;

let gen_sig_alias ~loc ~type_name ~base =
  let t_typ = ptyp_constr ~loc { txt = Ldot (Ldot (base, "Option"), "t"); loc } [] in
  gen_sig_items ~loc ~type_name ~t_typ
;;

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

(* The deriver args: { none = <expr>; sentinel = true; is_none = <expr> } *)
let make_args () =
  let open Deriving.Args in
  empty
  +> arg
       "none"
       Ast_pattern.(as__ (pexp_constant drop ||| pexp_record_unboxed_product drop drop))
  +> arg "sentinel" Ast_pattern.__
  +> arg "is_none" Ast_pattern.__
;;

let str_type_decl =
  Deriving.Generator.V2.make
    (make_args ())
    (fun ~ctxt (_rec_flag, tds) none_opt sentinel_opt is_none_opt ->
       let loc = Expansion_context.Deriver.derived_item_loc ctxt in
       match tds with
       | [ td ] ->
         let type_name = td.ptype_name.txt in
         let sentinel_override =
           match sentinel_opt with
           | None -> false
           | Some expr -> parse_bool_literal ~loc ~field_name:"sentinel" expr
         in
         let type_info = detect_type_info ~loc td in
         let items =
           match type_info with
           | Alias base -> gen_str_alias ~loc ~type_name ~base
           | Payload pti ->
             gen_str_option
               ~loc
               ~type_name
               ~type_info:pti
               ~none_override:none_opt
               ~sentinel_override
               ~is_none_override:is_none_opt
         in
         [ wrap_in_module_str ~loc "Option" items ]
       | _ ->
         Location.raise_errorf
           ~loc
           "ppx_uopt: only single type declarations are supported")
;;

let sig_type_decl =
  Deriving.Generator.V2.make
    (make_args ())
    (fun ~ctxt (_rec_flag, tds) none_opt sentinel_opt _ ->
       let loc = Expansion_context.Deriver.derived_item_loc ctxt in
       match tds with
       | [ td ] ->
         let type_name = td.ptype_name.txt in
         let sentinel_override =
           match sentinel_opt with
           | None -> false
           | Some expr -> parse_bool_literal ~loc ~field_name:"sentinel" expr
         in
         let type_info = detect_type_info ~loc td in
         let items =
           match type_info with
           | Alias base -> gen_sig_alias ~loc ~type_name ~base
           | Payload _ ->
             gen_sig_option ~loc ~type_name ~none_override:none_opt ~sentinel_override
         in
         [ wrap_in_module_sig ~loc "Option" items ]
       | _ ->
         Location.raise_errorf
           ~loc
           "ppx_uopt: only single type declarations are supported")
;;

let () = Deriving.add "unboxed_option" ~str_type_decl ~sig_type_decl |> Deriving.ignore
