open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers

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

(* [Stdlib_stable.<Mod>.min_int ()] - the small-int unboxed scalars expose [min_int] as a
   nullary function rather than a value. *)
let stdlib_stable_min_int ~loc mod_name =
  eapply ~loc (evar ~loc ("Stdlib_stable." ^ mod_name ^ ".min_int")) [ [%expr ()] ]
;;

let default_none_expr ~loc = function
  | Float_u_scalar -> Some (evar ~loc "Float_u.nan")
  | Float32_u_scalar -> Some (evar ~loc "Float32_u.nan")
  | Int32_u_scalar -> Some (evar ~loc "Int32_u.min_value")
  | Int64_u_scalar -> Some (evar ~loc "Int64_u.min_value")
  | Nativeint_u_scalar -> Some (evar ~loc "Nativeint_u.min_value")
  | Int8_u_scalar -> Some (stdlib_stable_min_int ~loc "Int8_u")
  | Int16_u_scalar -> Some (stdlib_stable_min_int ~loc "Int16_u")
  | Int_u_scalar -> Some (stdlib_stable_min_int ~loc "Int_u")
  | Char_u_scalar -> None
;;

let none_override_expr ~loc kind expr =
  let expr = to_unboxed_constant_expr ~loc expr in
  match kind with
  | Int_u_scalar ->
    (* [int#] has no direct constant syntax, so a tagged-int literal like [0] gets wrapped
       via [%int#_of_int]. Any other expression is passed through and must already be of
       type [int#]. *)
    (match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
     | Pexp_constant c ->
       (match Ppxlib_jane.Shim.Constant.of_parsetree c with
        | Pconst_integer (_, None) -> eapply ~loc (evar ~loc "_uopt_of_int") [ expr ]
        | _ -> expr)
     | _ -> expr)
  | _ -> expr
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
  | Float_u_scalar -> evar ~loc "Float_u.nan"
  | Float32_u_scalar -> evar ~loc "Float32_u.nan"
  | Int32_u_scalar -> to_unboxed_constant_expr ~loc [%expr 0l]
  | Int64_u_scalar -> to_unboxed_constant_expr ~loc [%expr 0L]
  | Nativeint_u_scalar -> to_unboxed_constant_expr ~loc [%expr 0n]
  | Int8_u_scalar -> to_unboxed_constant_expr ~loc [%expr 0s]
  | Int16_u_scalar -> to_unboxed_constant_expr ~loc [%expr 0S]
  | Int_u_scalar -> eapply ~loc (evar ~loc "_uopt_of_int") [ [%expr 0] ]
  | Char_u_scalar -> to_unboxed_constant_expr ~loc [%expr '\000']
;;

let equal_fn ~loc = function
  | Float_u_scalar -> evar ~loc "Float_u.equal"
  | Float32_u_scalar -> evar ~loc "Float32_u.equal"
  | Int32_u_scalar -> evar ~loc "Int32_u.equal"
  | Int64_u_scalar -> evar ~loc "Int64_u.equal"
  | Nativeint_u_scalar -> evar ~loc "Nativeint_u.equal"
  | Int8_u_scalar -> evar ~loc "_uopt_equal_int8"
  | Int16_u_scalar -> evar ~loc "_uopt_equal_int16"
  | Int_u_scalar -> evar ~loc "_uopt_equal_int"
  | Char_u_scalar -> evar ~loc "_uopt_equal_char"
;;

(* Float kinds get NaN-detection: an override that's syntactically [Float_u.nan] /
   [Float32_u.nan] (or [Stdlib.]-prefixed) generates [<Mod>.is_nan t] instead of
   [<Mod>.equal t nan], which is always [false] by IEEE 754. *)
let nan_detection ~loc = function
  | Float_u_scalar ->
    Some (evar ~loc "Float_u.is_nan", [ "Float_u.nan"; "Stdlib.Float_u.nan" ])
  | Float32_u_scalar ->
    Some (evar ~loc "Float32_u.is_nan", [ "Float32_u.nan"; "Stdlib.Float32_u.nan" ])
  | _ -> None
;;

let is_none_body ~loc ~kind ~none_override t_expr =
  match none_override, nan_detection ~loc kind with
  | Some override, Some (is_nan, paths) when expr_is_qualified_ident ~loc override paths
    -> eapply ~loc is_nan [ t_expr ]
  | _ -> eapply ~loc (equal_fn ~loc kind) [ t_expr; none_expr ~loc ~kind ~none_override ]
;;

let sexp_of_value_expr ~loc kind value_expr =
  let atom s = [%expr Sexplib0.Sexp.Atom [%e s]] in
  (* Modules with their own [sexp_of_t] - delegate via the heap/stack alloc-var attribute
     so the generated [sexp_of_value] matches its templated signature. *)
  let sexp_of_t_via mod_name =
    eapply ~loc (with_alloc_var ~loc (evar ~loc (mod_name ^ ".sexp_of_t"))) [ value_expr ]
  in
  (* Small-int kinds don't have a typed [sexp_of_t]; convert to [int] then to a string. *)
  let int_atom_of to_int_path =
    atom [%expr Stdlib.string_of_int ([%e evar ~loc to_int_path] [%e value_expr])]
  in
  match kind with
  | Float_u_scalar -> sexp_of_t_via "Float_u"
  | Float32_u_scalar -> sexp_of_t_via "Float32_u"
  | Int32_u_scalar -> sexp_of_t_via "Int32_u"
  | Int64_u_scalar -> sexp_of_t_via "Int64_u"
  | Nativeint_u_scalar -> sexp_of_t_via "Nativeint_u"
  | Int8_u_scalar -> int_atom_of "Stdlib_stable.Int8_u.to_int"
  | Int16_u_scalar -> int_atom_of "Stdlib_stable.Int16_u.to_int"
  | Int_u_scalar -> int_atom_of "Stdlib_stable.Int_u.to_int"
  | Char_u_scalar ->
    atom
      [%expr
        Stdlib.String.make
          1
          ([%e evar ~loc "Stdlib_stable.Char_u.to_char"] [%e value_expr])]
;;

(* Primitive externals requested by individual scalar kinds. Kept as an enum so the
   aggregator in [Ppx_uopt] can dedup across the kinds appearing in an unboxed record
   (e.g. [int8#] and [char#] both want [_uopt_equal_int8]). *)
type primitive =
  | Equal_int8
  | Equal_int16
  | Of_int
  | Equal_int
  | Char_to_int8

let compare_primitive a b = Stdlib.compare a b

let primitive_item ~loc = function
  | Equal_int8 ->
    [%stri external _uopt_equal_int8 : int8# -> int8# -> bool = "%int8#_equal"]
  | Equal_int16 ->
    [%stri external _uopt_equal_int16 : int16# -> int16# -> bool = "%int16#_equal"]
  | Of_int -> [%stri external _uopt_of_int : int -> int# = "%int#_of_int"]
  | Equal_int -> [%stri external _uopt_equal_int : int# -> int# -> bool = "%int#_equal"]
  | Char_to_int8 -> [%stri external _uopt_char_to_int8 : char# -> int8# = "%identity"]
;;

let primitives_for_kind = function
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar -> []
  | Int8_u_scalar -> [ Equal_int8 ]
  | Int16_u_scalar -> [ Equal_int16 ]
  | Int_u_scalar -> [ Of_int; Equal_int ]
  (* [char#] doesn't expose a primitive equality, so go via [int8#]: convert each operand
     with the [%identity] cast, then compare with [%int8#_equal]. *)
  | Char_u_scalar -> [ Equal_int8; Char_to_int8 ]
;;

(* Non-primitive bindings (regular [let] definitions). Currently only [_uopt_equal_char]
   needs one; everything else is exhausted by primitives. *)
let extra_bindings ~loc = function
  | Float_u_scalar
  | Float32_u_scalar
  | Int32_u_scalar
  | Int64_u_scalar
  | Nativeint_u_scalar
  | Int8_u_scalar
  | Int16_u_scalar
  | Int_u_scalar -> []
  | Char_u_scalar ->
    [ let_inline
        ~loc
        "_uopt_equal_char"
        [%expr fun x y -> _uopt_equal_int8 (_uopt_char_to_int8 x) (_uopt_char_to_int8 y)]
    ]
;;
