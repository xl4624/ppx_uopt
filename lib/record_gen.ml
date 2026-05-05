open Ppxlib
open Ast_builder.Default
open Uopt_types
module Ah = Ast_helpers
module C = Classification

let contract_payload_name field_name = "_uopt_contract_" ^ field_name ^ "_payload"
let contract_is_none_name field_name = "_uopt_contract_" ^ field_name ^ "_is_none"

let immediate_equal_fn ~loc = function
  | Imm_int -> Ah.eqident ~loc [ "Stdlib"; "Int"; "equal" ]
  | Imm_bool -> Ah.eqident ~loc [ "Stdlib"; "Bool"; "equal" ]
  | Imm_char -> Ah.eqident ~loc [ "Stdlib"; "Char"; "equal" ]
  | Imm_float -> Ah.eqident ~loc [ "Stdlib"; "Float"; "equal" ]
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
    let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
    (match desc with
     | Pexp_record_unboxed_product (override_fields, None) ->
       let fields =
         List.map
           (fun (field_lid, field_expr) ->
             let field_name =
               match field_lid with
               | { txt = Lident s; _ } -> s
               | _ ->
                 Location.raise_errorf ~loc "ppx_uopt: expected an unqualified field name"
             in
             field_name, field_expr)
           override_fields
       in
       let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
       let rec check_dups = function
         | (a, _) :: ((b, _) :: _ as rest) ->
           if String.equal a b
           then
             Location.raise_errorf
               ~loc
               "ppx_uopt: duplicate none override for unboxed-record field '%s'"
               a;
           check_dups rest
         | _ -> ()
       in
       check_dups sorted;
       fields
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
                    "ppx_uopt: unboxed-record field '%s' of type %s requires an explicit \
                     none sentinel for the whole record"
                    field_name
                    (Scalar_gen.kind_name kind))
             | Record_field_contract _ -> Ah.evar ~loc (contract_payload_name field_name)
             | Record_field_immediate imm -> immediate_default_expr ~loc imm
             | Record_field_opaque field_type ->
               (* Field is not in the override and its type has no synthesised default, so
                  it's payload-only: [is_none] never inspects it. Use [Obj.magic 0] as a
                  never-observed placeholder. *)
               Ah.opaque_default_payload_expr ~loc field_type)
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
    | Record_field_scalar _ | Record_field_immediate _ | Record_field_opaque _ -> []
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
                   (Ah.eqident_lid ~loc base [ "Option"; "unsafe_value" ])
                   [ Ah.eqident_lid ~loc base [ "Option"; "none" ] ])
          ]
      ]
      @
      if need_is_none
      then
        [ pstr_value
            ~loc
            Nonrecursive
            [ Ah.mk_val_binding
                ~loc
                (contract_is_none_name field_name)
                (Ah.fun_one
                   ~loc
                   "v"
                   (Ah.eapply
                      ~loc
                      (Ah.eqident_lid ~loc base [ "Option"; "is_none" ])
                      [ Ah.evar ~loc "v" ]))
            ]
        ]
      else [])
;;

(* True iff the sentinel-mode [is_none] for these labels would use [Stdlib.( = )] on an
   opaque field. Such uses lower to [caml_equal], which the static [@zero_alloc] checker
   conservatively treats as potentially-allocating, so callers emit [@@zero_alloc assume]
   on the affected functions. *)
let unboxed_record_is_none_uses_poly_eq ~loc labels ~none_override =
  let override_exprs = unboxed_record_none_overrides ~loc none_override in
  List.exists
    (fun (ld : label_declaration) ->
      match C.classify_record_field ~loc ld with
      | Record_field_opaque _ -> List.mem_assoc ld.pld_name.txt override_exprs
      | Record_field_scalar _ | Record_field_contract _ | Record_field_immediate _ ->
        false)
    labels
;;

(* [is_none] checks only the fields the user listed in [none = #{ ... }]. Fields omitted
   from the override are payload-only and may freely take any value. Each listed field
   acts as a sentinel discriminator. *)
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
  let labels_to_check =
    List.filter
      (fun (ld : label_declaration) -> List.mem_assoc ld.pld_name.txt override_exprs)
      labels
  in
  let checks =
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
            [ field_access ]
        | Record_field_immediate imm ->
          let override_expr = List.assoc field_name override_exprs in
          (* For [Imm_float], detect a NaN override and emit [Float.is_nan] instead of
             [Float.equal] (which is always [false] against NaN by IEEE 754). *)
          (match imm with
           | Imm_float
             when Ah.expr_is_qualified_ident
                    ~loc
                    override_expr
                    [ "Float.nan"; "Stdlib.Float.nan" ] ->
             Ah.eapply ~loc (Ah.eqident ~loc [ "Stdlib"; "Float"; "is_nan" ]) [ field_access ]
           | _ ->
             Ah.eapply ~loc (immediate_equal_fn ~loc imm) [ field_access; override_expr ])
        | Record_field_opaque _ ->
          let override_expr = List.assoc field_name override_exprs in
          [%expr Stdlib.( = ) [%e field_access] [%e override_expr]])
      labels_to_check
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
