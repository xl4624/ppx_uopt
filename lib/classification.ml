open Ppxlib
open Uopt_types

(* Recognise either an unqualified type constructor [Lident "x"] or a
   single-module-qualified one [Ldot (Lident "M", "x")] with no type args. Returns the
   path as a dotted string for table lookup. Deeper qualifications (e.g. [A.B.t]) fall
   through to [None]. *)
let nullary_constr_path (ct : core_type) : string option =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident s; _ }, []) -> Some s
  | Ptyp_constr ({ txt = Ldot (Lident m, s); _ }, []) -> Some (m ^ "." ^ s)
  | _ -> None
;;

let scalar_kind_of_core_type ct =
  match nullary_constr_path ct with
  | Some ("float#" | "Float_u.t") -> Some Float_u_scalar
  | Some ("float32#" | "Float32_u.t") -> Some Float32_u_scalar
  | Some ("int32#" | "Int32_u.t") -> Some Int32_u_scalar
  | Some ("int64#" | "bits64" | "Int64_u.t" | "Bits64.t") -> Some Int64_u_scalar
  | Some ("nativeint#" | "Nativeint_u.t") -> Some Nativeint_u_scalar
  | Some "int8#" -> Some Int8_u_scalar
  | Some "int16#" -> Some Int16_u_scalar
  | Some "int#" -> Some Int_u_scalar
  | Some "char#" -> Some Char_u_scalar
  | _ -> None
;;

let immediate_kind_of_core_type ct =
  match nullary_constr_path ct with
  | Some "int" -> Some Imm_int
  | Some "bool" -> Some Imm_bool
  | Some "char" -> Some Imm_char
  | Some "float" -> Some Imm_float
  | _ -> None
;;

(* [Some base] iff [ct] is [M.t] for contract fields and module-qualified aliases. *)
let module_t_base (ct : core_type) =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ct.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (base, "t"); _ }, []) -> Some base
  | _ -> None
;;

let classify_record_field ~loc:_ (ld : label_declaration) =
  match scalar_kind_of_core_type ld.pld_type with
  | Some kind -> Record_field_scalar kind
  | None ->
    (match immediate_kind_of_core_type ld.pld_type with
     | Some imm -> Record_field_immediate imm
     | None ->
       (match module_t_base ld.pld_type with
        | Some base -> Record_field_contract base
        (* Anything else is treated as an opaque value-layout field. Tagged mode
           synthesises a placeholder via [Stdlib.Obj.magic 0]. *)
        | None -> Record_field_opaque ld.pld_type))
;;

let detect_type_info ~loc (td : type_declaration) =
  match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
  | Ptype_record_unboxed_product labels -> Payload (Unboxed_record labels)
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: abstract type with no manifest is not supported"
     | Some ct ->
       (match scalar_kind_of_core_type ct, module_t_base ct with
        | Some kind, _ -> Payload (Scalar kind)
        | None, Some base -> Alias base
        | None, None ->
          Location.raise_errorf
            ~loc
            "ppx_uopt: unsupported type. Supported types are unboxed scalars (float#, \
             float32#, int32#, int64#/bits64, nativeint#, int8#, int16#, int#, char#), \
             unboxed records, and module-qualified aliases of the form M.t."))
  | _ ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: only unboxed record products and unboxed scalar types are supported"
;;

let detect_sig_info ~loc (td : type_declaration) =
  match td.ptype_manifest with
  | Some ct ->
    (match module_t_base ct with
     | Some base -> `Alias base
     | None -> `Payload)
  | None ->
    (match Ppxlib_jane.Shim.Type_declaration.extract_jkind_annotation td with
     | Some _ -> `Payload
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: abstract type with no manifest or jkind annotation is not supported")
;;
