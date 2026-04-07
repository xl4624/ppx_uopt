open Ppxlib
open Ast_builder.Default
open Uopt_types
module Ah = Ast_helpers
module C = Classification

let contract_payload_name field_name = "_uopt_contract_" ^ field_name ^ "_payload"
let contract_is_none_name field_name = "_uopt_contract_" ^ field_name ^ "_is_none"

let unboxed_record_none_overrides ~loc = function
  | None -> []
  | Some expr ->
    let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
    (match desc with
     | Pexp_record_unboxed_product (override_fields, None) ->
       let fields =
         List.map
           (fun (field_lid, field_expr) ->
             let field_name =
               (fun ~loc -> function
                 | { txt = Lident s; _ } -> s
                 | _ ->
                   Location.raise_errorf
                     ~loc
                     "ppx_uopt: expected an unqualified field name")
                 ~loc
                 field_lid
             in
             field_name, field_expr)
           override_fields
       in
       let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
       (match sorted_fields with
        | (f, _) :: rest ->
          let _ =
            List.fold_left
              (fun prev_f (f, _) ->
                if String.equal prev_f f
                then
                  Location.raise_errorf
                    ~loc
                    "ppx_uopt: duplicate none override for unboxed-record field '%s'"
                    f
                else f)
              f
              rest
          in
          fields
        | [] -> [])
     | Pexp_record_unboxed_product (_, Some _) ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unboxed-record none sentinel does not support `with`"
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unboxed-record none sentinel must be an unboxed record literal, e.g. \
          #{ x = #0s }")
;;

let gen_unboxed_record_none ~loc labels ~none_override =
  match none_override with
  | Some _ | None ->
    let override_exprs = unboxed_record_none_overrides ~loc none_override in
    let fields =
      List.map
        (fun (ld : label_declaration) ->
          let field_name = ld.pld_name.txt in
          let field_lid = { txt = Lident field_name; loc } in
          let field_none =
            match List.assoc_opt field_name override_exprs with
            | Some expr -> expr
            | None ->
              (match C.classify_record_field ~loc ld with
               | Record_field_scalar kind ->
                 (match Scalar_gen.default_none_expr ~loc kind with
                  | Some expr -> expr
                  | None ->
                    Location.raise_errorf
                      ~loc
                      "ppx_uopt: unboxed-record field '%s' of type %s requires an \
                       explicit none sentinel for the whole record"
                      field_name
                      (Scalar_gen.kind_name kind))
               | Record_field_contract _ ->
                 Ah.evar ~loc (contract_payload_name field_name))
          in
          field_lid, field_none)
        labels
    in
    pexp_record_unboxed_product ~loc fields None
;;

let contract_helper_items ~loc labels ~need_is_none =
  labels
  |> List.concat_map (fun (ld : label_declaration) ->
    let field_name = ld.pld_name.txt in
    match C.classify_record_field ~loc ld with
    | Record_field_scalar _ -> []
    | Record_field_contract base ->
      [ pstr_value
          ~loc
          Nonrecursive
          [ value_binding
              ~loc
              ~pat:
                (ppat_constraint
                   ~loc
                   (ppat_var ~loc { txt = contract_payload_name field_name; loc })
                   ld.pld_type)
              ~expr:
                (Ah.eapply
                   ~loc
                   (Ah.eqident_lid ~loc base [ "Option"; "unchecked_value" ])
                   [ Ah.eqident_lid ~loc base [ "Option"; "none" ] ])
          ]
      ]
      @
      if need_is_none
      then
        [ pstr_value
            ~loc
            Nonrecursive
            [ value_binding
                ~loc
                ~pat:
                  (ppat_constraint
                     ~loc
                     (ppat_var ~loc { txt = contract_is_none_name field_name; loc })
                     (ptyp_arrow ~loc Nolabel ld.pld_type [%type: bool]))
                ~expr:(Ah.eqident_lid ~loc base [ "Option"; "is_none" ])
            ]
        ]
      else [])
;;

let gen_unboxed_record_is_none_sentinel ~loc labels ~none_override t_expr =
  let override_exprs = unboxed_record_none_overrides ~loc none_override in
  let checks =
    let () =
      List.iter
        (fun (name, _) ->
          if not (List.exists (fun ld -> ld.pld_name.txt = name) labels)
          then
            Location.raise_errorf
              ~loc
              "ppx_uopt: field '%s' from none override not found in type"
              name)
        override_exprs
    in
    List.map
      (fun (ld : label_declaration) ->
        let field_name = ld.pld_name.txt in
        let field_access =
          pexp_unboxed_field ~loc t_expr { txt = Lident field_name; loc }
        in
        match C.classify_record_field ~loc ld with
        | Record_field_scalar kind ->
          let field_none_override = List.assoc_opt field_name override_exprs in
          Scalar_gen.is_none_body
            ~loc
            ~kind
            ~none_override:field_none_override
            field_access
        | Record_field_contract _ ->
          Ah.eapply
            ~loc
            (Ah.evar ~loc (contract_is_none_name field_name))
            [ field_access ])
      labels
  in
  match checks with
  | [] ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: cannot generate is_none for unboxed record with no checkable fields"
  | first :: rest ->
    List.fold_left (fun acc check -> [%expr [%e acc] && [%e check]]) first rest
;;
