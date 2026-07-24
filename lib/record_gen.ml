open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers
open Classification

let contract_payload_name field_name = "_uopt_contract_" ^ field_name ^ "_payload"
let contract_is_none_name field_name = "_uopt_contract_" ^ field_name ^ "_is_none"

let immediate_equal_fn ~loc = function
  | Imm_int -> evar ~loc "Stdlib.Int.equal"
  | Imm_bool -> evar ~loc "Stdlib.Bool.equal"
  | Imm_char -> evar ~loc "Stdlib.Char.equal"
  | Imm_float -> evar ~loc "Stdlib.Float.equal"
;;

let immediate_default_expr ~loc = function
  | Imm_int -> [%expr 0]
  | Imm_bool -> [%expr false]
  | Imm_char -> [%expr '\000']
  | Imm_float -> [%expr 0.]
;;

let unboxed_record_none_overrides ~loc = function
  | None -> []
  | Some expr ->
    (match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
     | Pexp_record_unboxed_product (override_fields, None) ->
       let pairs =
         List.map
           (fun (lid, e) ->
             match lid.txt with
             | Lident s -> s, e
             | _ ->
               Location.raise_errorf ~loc "ppx_uopt: expected an unqualified field name")
           override_fields
       in
       let _ : string list =
         List.fold_left
           (fun seen (name, _) ->
             if List.mem name seen
             then
               Location.raise_errorf
                 ~loc
                 "ppx_uopt: duplicate none override for unboxed-record field '%s'"
                 name;
             name :: seen)
           []
           pairs
       in
       pairs
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

let none_for_unoverridden_field ~loc ld =
  let field_name = ld.pld_name.txt in
  match classify_record_field ~loc ld with
  | Record_field_scalar kind ->
    (match Scalar_gen.default_none_expr ~loc kind with
     | Some expr -> expr
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unboxed-record field '%s' of type %s requires an explicit none \
          sentinel for the whole record"
         field_name
         (Scalar_gen.kind_name kind))
  | Record_field_contract _ -> evar ~loc (contract_payload_name field_name)
  | Record_field_immediate imm -> immediate_default_expr ~loc imm
  | Record_field_opaque field_type -> opaque_default_payload_expr ~loc field_type
;;

let gen_unboxed_record_none ~loc labels ~none_override =
  let override_exprs = unboxed_record_none_overrides ~loc none_override in
  let fields =
    List.map
      (fun (ld : label_declaration) ->
        let field_name = ld.pld_name.txt in
        let field_none =
          match List.assoc_opt field_name override_exprs with
          | Some expr -> expr
          | None -> none_for_unoverridden_field ~loc ld
        in
        Located.lident ~loc field_name, field_none)
      labels
  in
  pexp_record_unboxed_product ~loc fields None
;;

let contract_helper_items ~loc labels ~need_is_none =
  labels
  |> List.concat_map (fun (ld : label_declaration) ->
    match classify_record_field ~loc ld with
    | Record_field_contract base ->
      let field_name = ld.pld_name.txt in
      let m_option name = qual_evar ~loc base [ "Option"; name ] in
      let payload =
        let_def_pat
          ~loc
          (ppat_constraint
             ~loc
             (pvar ~loc (contract_payload_name field_name))
             ld.pld_type)
          [%expr [%e m_option "unsafe_value"] [%e m_option "none"]]
      in
      let is_none_helper =
        let_inline
          ~loc
          (contract_is_none_name field_name)
          [%expr fun v -> [%e m_option "is_none"] v]
      in
      payload :: (if need_is_none then [ is_none_helper ] else [])
    | Record_field_scalar _ | Record_field_immediate _ | Record_field_opaque _ -> [])
;;

let unboxed_record_is_none_uses_poly_eq ~loc labels ~none_override =
  let override_exprs = unboxed_record_none_overrides ~loc none_override in
  List.exists
    (fun (ld : label_declaration) ->
      match classify_record_field ~loc ld with
      | Record_field_opaque _ -> List.mem_assoc ld.pld_name.txt override_exprs
      | Record_field_scalar _ | Record_field_contract _ | Record_field_immediate _ ->
        false)
    labels
;;

let is_none_check_for_field ~loc t_expr override_exprs ld =
  let field_name = ld.pld_name.txt in
  let access = pexp_unboxed_field ~loc t_expr (Located.lident ~loc field_name) in
  match classify_record_field ~loc ld with
  | Record_field_scalar kind ->
    Scalar_gen.is_none_body
      ~loc
      ~kind
      ~none_override:(List.assoc_opt field_name override_exprs)
      access
  | Record_field_contract _ ->
    eapply ~loc (evar ~loc (contract_is_none_name field_name)) [ access ]
  | Record_field_immediate imm ->
    let override_expr = List.assoc field_name override_exprs in
    (* For [Imm_float], detect a NaN override and emit [Float.is_nan] instead of
       [Float.equal] (which is always [false] against NaN by IEEE 754). *)
    (match imm with
     | Imm_float
       when expr_is_qualified_ident ~loc override_expr [ "Float.nan"; "Stdlib.Float.nan" ]
       -> eapply ~loc (evar ~loc "Stdlib.Float.is_nan") [ access ]
     | _ -> eapply ~loc (immediate_equal_fn ~loc imm) [ access; override_expr ])
  | Record_field_opaque _ ->
    let override_expr = List.assoc field_name override_exprs in
    [%expr Stdlib.( = ) [%e access] [%e override_expr]]
;;

let gen_unboxed_record_is_none_sentinel ~loc labels ~none_override t_expr =
  let override_exprs = unboxed_record_none_overrides ~loc none_override in
  List.iter
    (fun (name, _) ->
      if not (List.exists (fun ld -> ld.pld_name.txt = name) labels)
      then
        Location.raise_errorf
          ~loc
          "ppx_uopt: field '%s' from none override not found in type"
          name)
    override_exprs;
  let checks =
    labels
    |> List.filter (fun ld -> List.mem_assoc ld.pld_name.txt override_exprs)
    |> List.map (is_none_check_for_field ~loc t_expr override_exprs)
  in
  match checks with
  | [] ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: [@@deriving unboxed_option { none = #{ ... } }] needs at least one \
       field in the override to discriminate [is_none]; got an empty record."
  | first :: rest ->
    List.fold_left (fun acc check -> [%expr [%e acc] && [%e check]]) first rest
;;
