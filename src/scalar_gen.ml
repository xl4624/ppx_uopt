open Ppxlib
open Ast_builder.Default
open Uopt_types
module Ah = Ast_helpers

let kind_name = function
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

let default_none_expr ~loc = function
  | Float_u_scalar ->
    Some
      (Ah.eapply
         ~loc
         (Ah.eqident ~loc [ "Float_u"; "nan" ])
         [ pexp_construct ~loc { txt = Lident "()"; loc } None ])
  | Float32_u_scalar ->
    Some
      (Ah.eapply
         ~loc
         (Ah.eqident ~loc [ "Float32_u"; "nan" ])
         [ pexp_construct ~loc { txt = Lident "()"; loc } None ])
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar
  | Char_u_scalar -> None
;;

let none_override_expr ~loc kind expr =
  match kind with
  | Int_u_scalar ->
    let expr = Ah.to_unboxed_constant_expr ~loc expr in
    let desc = Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc in
    (match desc with
     | Pexp_constant c ->
       (match Ppxlib_jane.Shim.Constant.of_parsetree c with
        | Pconst_integer (_, None) ->
          Ah.eapply ~loc (Ah.evar ~loc "_uopt_of_int") [ expr ]
        | _ ->
          Location.raise_errorf
            ~loc
            "ppx_uopt: int# none sentinel must be an int literal, e.g. [@@deriving \
             unboxed_option { none = 0 }]")
     | _ ->
       Location.raise_errorf ~loc "ppx_uopt: none sentinel must be a constant literal")
  | _ -> Ah.to_unboxed_constant_expr ~loc expr
;;

let none_expr ~loc ~kind ~none_override =
  match none_override with
  | Some expr -> none_override_expr ~loc kind expr
  | None ->
    (match default_none_expr ~loc kind with
     | Some expr -> expr
     | None ->
       Location.raise_errorf
         ~loc
         "ppx_uopt: %s requires a none sentinel, e.g. [@@deriving unboxed_option { none \
          = ... }]"
         (kind_name kind))
;;

let default_payload_expr ~loc = function
  | Float_u_scalar ->
    Ah.eapply
      ~loc
      (Ah.eqident ~loc [ "Float_u"; "nan" ])
      [ pexp_construct ~loc { txt = Lident "()"; loc } None ]
  | Float32_u_scalar ->
    Ah.eapply
      ~loc
      (Ah.eqident ~loc [ "Float32_u"; "nan" ])
      [ pexp_construct ~loc { txt = Lident "()"; loc } None ]
  | Int32_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr 0l]
  | Int64_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr 0L]
  | Nativeint_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr 0n]
  | Int8_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr 0s]
  | Int16_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr 0S]
  | Int_u_scalar -> Ah.eapply ~loc (Ah.evar ~loc "_uopt_of_int") [ [%expr 0] ]
  | Char_u_scalar -> Ah.to_unboxed_constant_expr ~loc [%expr '\000']
;;

let equal_fn ~loc = function
  | Float_u_scalar -> Ah.eqident ~loc [ "Float_u"; "equal" ]
  | Float32_u_scalar -> Ah.eqident ~loc [ "Float32_u"; "equal" ]
  | Int32_u_scalar -> Ah.eqident ~loc [ "Int32_u"; "equal" ]
  | Int64_u_scalar -> Ah.eqident ~loc [ "Int64_u"; "equal" ]
  | Nativeint_u_scalar -> Ah.eqident ~loc [ "Nativeint_u"; "equal" ]
  | Int8_u_scalar -> Ah.evar ~loc "_uopt_equal_int8"
  | Int16_u_scalar -> Ah.evar ~loc "_uopt_equal_int16"
  | Int_u_scalar -> Ah.evar ~loc "_uopt_equal_int"
  | Char_u_scalar -> Ah.evar ~loc "_uopt_equal_char"
;;

let is_none_body ~loc ~kind ~none_override t_expr =
  match kind, none_override with
  | Float_u_scalar, None ->
    Ah.eapply ~loc (Ah.eqident ~loc [ "Float_u"; "is_nan" ]) [ t_expr ]
  | Float32_u_scalar, None ->
    Ah.eapply ~loc (Ah.eqident ~loc [ "Float32_u"; "is_nan" ]) [ t_expr ]
  | _ ->
    let sentinel = none_expr ~loc ~kind ~none_override in
    Ah.eapply ~loc (equal_fn ~loc kind) [ t_expr; sentinel ]
;;

let helper_items ~loc = function
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar -> []
  | Int8_u_scalar ->
    [ Ah.primitive_sig
        ~loc
        "_uopt_equal_int8"
        [%type: int8# -> int8# -> bool]
        "%int8#_equal"
    ]
  | Int16_u_scalar ->
    [ Ah.primitive_sig
        ~loc
        "_uopt_equal_int16"
        [%type: int16# -> int16# -> bool]
        "%int16#_equal"
    ]
  | Int_u_scalar ->
    [ Ah.primitive_sig ~loc "_uopt_of_int" [%type: int -> int#] "%int#_of_int"
    ; Ah.primitive_sig ~loc "_uopt_equal_int" [%type: int# -> int# -> bool] "%int#_equal"
    ]
  | Char_u_scalar ->
    [ Ah.primitive_sig
        ~loc
        "_uopt_equal_int8"
        [%type: int8# -> int8# -> bool]
        "%int8#_equal"
    ; Ah.primitive_sig ~loc "_uopt_char_to_int8" [%type: char# -> int8#] "%identity"
    ; pstr_value
        ~loc
        Nonrecursive
        [ Ah.mk_val_binding
            ~loc
            "_uopt_equal_char"
            (Ah.fun_one
               ~loc
               "x"
               (Ah.fun_one
                  ~loc
                  "y"
                  (Ah.eapply
                     ~loc
                     (Ah.evar ~loc "_uopt_equal_int8")
                     [ Ah.eapply
                         ~loc
                         (Ah.evar ~loc "_uopt_char_to_int8")
                         [ Ah.evar ~loc "x" ]
                     ; Ah.eapply
                         ~loc
                         (Ah.evar ~loc "_uopt_char_to_int8")
                         [ Ah.evar ~loc "y" ]
                     ])))
        ]
    ]
;;
