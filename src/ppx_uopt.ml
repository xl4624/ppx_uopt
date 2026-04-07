open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers
open Classification

let repr_kind_of_options ~none_override ~sentinel_override =
  if Option.is_some none_override || sentinel_override then Sentinel_repr else Tagged_repr
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
          ~manifest:(Some (ptyp_constr ~loc { txt = Lident type_name; loc } []))
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
  let msg = estring ~loc (Printf.sprintf "%s.Option.value_exn: none" type_name) in
  let nothing_ty = ptyp_constr ~loc { txt = Lident "_uopt_empty"; loc } [] in
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
  @ contract_items
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

let gen_sig_option ~loc ~type_name ~none_override ~sentinel_override =
  let manifest_typ = ptyp_constr ~loc { txt = Lident type_name; loc } [] in
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
  let t_typ =
    match repr_kind_of_options ~none_override ~sentinel_override with
    | Sentinel_repr -> value_typ
    | Tagged_repr -> tagged_option_type ~loc
  in
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
let args =
  let open Deriving.Args in
  empty
  +> arg
       "none"
       Ast_pattern.(as__ (pexp_constant drop ||| pexp_record_unboxed_product drop drop))
  +> arg "sentinel" Ast_pattern.__
  +> arg "is_none" Ast_pattern.__
;;

let str_type_decl =
  Deriving.Generator.make
    args
    (fun ~loc ~path:_ (_rec_flag, tds) none_opt sentinel_opt is_none_opt ->
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
           gen_str_option
             ~loc
             ~type_name
             ~type_info
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

let sig_args =
  let open Deriving.Args in
  empty
  +> arg
       "none"
       Ast_pattern.(as__ (pexp_constant drop ||| pexp_record_unboxed_product drop drop))
  +> arg "sentinel" Ast_pattern.__
  +> arg "is_none" Ast_pattern.__
;;

let sig_type_decl =
  Deriving.Generator.make
    sig_args
    (fun ~loc ~path:_ (_rec_flag, tds) none_opt sentinel_opt _ ->
       match tds with
       | [ td ] ->
         let type_name = td.ptype_name.txt in
         let sentinel_override =
           match sentinel_opt with
           | None -> false
           | Some expr -> parse_bool_literal ~loc ~field_name:"sentinel" expr
         in
         let items =
           gen_sig_option ~loc ~type_name ~none_override:none_opt ~sentinel_override
         in
         [ wrap_in_module_sig ~loc "Option" items ]
       | _ ->
         Location.raise_errorf
           ~loc
           "ppx_uopt: only single type declarations are supported")
;;

let () = Deriving.add "unboxed_option" ~str_type_decl ~sig_type_decl |> Deriving.ignore
