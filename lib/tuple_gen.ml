open Ppxlib
open Ast_builder.Default
open Uopt_types

let component_var_name i = Printf.sprintf "_uopt_c%d" i

let require_override ~loc = function
  | Some expr -> expr
  | None ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: internal error: sentinel-mode unboxed-tuple none requires an override"
;;

(* Returns the raw override expression per component, in order; callers normalize each one
   via [Scalar_gen.none_expr] / [Scalar_gen.is_none_body]. *)
let parse_tuple_none_override ~loc ~arity none_override =
  let expr = require_override ~loc none_override in
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
  | Pexp_unboxed_tuple components ->
    let n = List.length components in
    if n <> arity
    then
      Location.raise_errorf
        ~loc
        "ppx_uopt: none sentinel has %d component(s), expected %d to match the tuple \
         payload type"
        n
        arity;
    List.map
      (fun (label, e) ->
        if Option.is_some label
        then
          Location.raise_errorf
            ~loc
            "ppx_uopt: labeled unboxed-tuple components are not supported";
        e)
      components
  | _ ->
    Location.raise_errorf
      ~loc
      "ppx_uopt: unboxed-tuple none sentinel must be an unboxed tuple literal, e.g. \
       #(#0s, #0l)"
;;

let destructuring_pattern ~loc vars =
  ppat_unboxed_tuple ~loc (List.map (fun v -> None, pvar ~loc v) vars) Closed
;;

let gen_unboxed_tuple_none ~loc kinds ~none_override =
  let overrides =
    parse_tuple_none_override ~loc ~arity:(List.length kinds) none_override
  in
  let components =
    List.map2
      (fun kind override ->
        None, Scalar_gen.none_expr ~loc ~kind ~none_override:(Some override))
      kinds
      overrides
  in
  pexp_unboxed_tuple ~loc components
;;

let gen_unboxed_tuple_is_none ~loc kinds ~none_override t_expr =
  let overrides =
    parse_tuple_none_override ~loc ~arity:(List.length kinds) none_override
  in
  let vars = List.mapi (fun i (_ : scalar_kind) -> component_var_name i) kinds in
  let checks =
    List.map2
      (fun (kind, override) var ->
        Scalar_gen.is_none_body ~loc ~kind ~none_override:(Some override) (evar ~loc var))
      (List.combine kinds overrides)
      vars
  in
  match checks with
  | [] ->
    (* Unreachable: unboxed tuples have arity >= 2, so [kinds] is never empty. *)
    Location.raise_errorf ~loc "ppx_uopt: internal error: empty unboxed-tuple payload"
  | first :: rest ->
    let body =
      List.fold_left (fun acc check -> [%expr [%e acc] && [%e check]]) first rest
    in
    pexp_match
      ~loc
      t_expr
      [ case ~lhs:(destructuring_pattern ~loc vars) ~guard:None ~rhs:body ]
;;

let gen_unboxed_tuple_sexp_of_value ~loc kinds v_expr =
  let vars = List.mapi (fun i (_ : scalar_kind) -> component_var_name i) kinds in
  let sexps =
    List.map2
      (fun kind var -> Scalar_gen.sexp_of_value_expr ~loc kind (evar ~loc var))
      kinds
      vars
  in
  pexp_match
    ~loc
    v_expr
    [ case
        ~lhs:(destructuring_pattern ~loc vars)
        ~guard:None
        ~rhs:[%expr Sexplib0.Sexp.List [%e elist ~loc sexps]]
    ]
;;
