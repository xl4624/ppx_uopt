open Ppxlib
open Uopt_types

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

let classify_record_field ~loc (ld : label_declaration) =
  let field_name = ld.pld_name.txt in
  match scalar_kind_of_core_type ld.pld_type with
  | Some scalar_kind -> Record_field_scalar scalar_kind
  | None ->
    let ct_desc = Ppxlib_jane.Shim.Core_type_desc.of_parsetree ld.pld_type.ptyp_desc in
    (match ct_desc with
     | Ptyp_constr ({ txt = Ldot (base, "t"); _ }, []) -> Record_field_contract base
     | Ptyp_constr ({ txt = Ldot (base, _); _ }, []) ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unsupported field type for field '%s'. Module-qualified contract \
          fields must have the form %s.t and provide an Option module where \
          %s.Option.unchecked_value %s.Option.none : %s.t."
         field_name
         (Ast_helpers.string_of_longident base)
         (Ast_helpers.string_of_longident base)
         (Ast_helpers.string_of_longident base)
         (Ast_helpers.string_of_longident base)
     | Ptyp_constr ({ txt = Lident _; _ }, []) ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unsupported field type for field '%s'. Supported unboxed-record \
          fields are supported unboxed scalar types or module-qualified contract types \
          of the form M.t with M.Option.none and M.Option.unchecked_value."
         field_name
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: unsupported field type for field '%s'. Supported unboxed-record \
          fields are supported unboxed scalar types or module-qualified contract types \
          of the form M.t with M.Option.none and M.Option.unchecked_value."
         field_name)
;;

let detect_type_info ~loc (td : type_declaration) =
  let kind = Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind in
  match kind with
  | Ptype_record_unboxed_product labels -> Unboxed_record labels
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | Some ct ->
       (match scalar_kind_of_core_type ct with
        | Some scalar_kind -> Scalar scalar_kind
        | None ->
          Location.raise_errorf
            ~loc
            "ppx_uopt: unsupported type. Supported scalar types are float#, float32#, \
             int32#, int64#/bits64, nativeint#, int8#, int16#, int#, char#, and unboxed \
             records.")
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: abstract type with no manifest is not supported")
  | _ ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: only unboxed record products and unboxed scalar types are supported"
;;
