open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers
open Classification

(* ===== shared shape helpers ===== *)

let let_def ~loc name body =
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding ~loc ~pat:(ppat_var ~loc { txt = name; loc }) ~expr:body ]
;;

let let_inline ?(assume_zero_alloc = false) ~loc name body =
  pstr_value ~loc Nonrecursive [ mk_val_binding ~assume_zero_alloc ~loc name body ]
;;

let let_inline_t ?assume_zero_alloc ~loc name body =
  let_inline ?assume_zero_alloc ~loc name (fun_one ~loc "t" body)
;;

let abstract_type ~loc name manifest =
  pstr_type
    ~loc
    Nonrecursive
    [ type_declaration
        ~loc
        ~name:{ txt = name; loc }
        ~params:[]
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some manifest)
    ]
;;

let abstract_sig_type ~loc name manifest =
  psig_type
    ~loc
    Nonrecursive
    [ type_declaration
        ~loc
        ~name:{ txt = name; loc }
        ~params:[]
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some manifest)
    ]
;;

let wrap_in_module_str ~loc name items =
  pstr_module
    ~loc
    (module_binding ~loc ~name:{ txt = Some name; loc } ~expr:(pmod_structure ~loc items))
;;

let wrap_in_constrained_module_str ~loc name ~sig_items ~str_items =
  pstr_module
    ~loc
    (module_binding
       ~loc
       ~name:{ txt = Some name; loc }
       ~expr:
         (pmod_constraint
            ~loc
            (pmod_structure ~loc str_items)
            (pmty_signature ~loc sig_items)))
;;

let wrap_in_module_sig ~loc name items =
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:{ txt = Some name; loc }
       ~type_:(pmty_signature ~loc items))
;;

let templated_value_binding ~loc ~name ~arg ~body =
  { (value_binding
       ~loc
       ~pat:(ppat_var ~loc { txt = name; loc })
       ~expr:(fun_one ~loc arg (with_exclave_if_stack ~loc body)))
    with
    pvb_attributes = [ alloc_heap_stack_attr ~loc; zero_alloc_ignore_attr ~loc ]
  }
;;

let let_template ~loc ~name ~arg ~body =
  pstr_template
    ~loc
    (pstr_value ~loc Nonrecursive [ templated_value_binding ~loc ~name ~arg ~body ])
;;

(* ===== generated boilerplate ===== *)

let suppress_warnings ~loc =
  (* The generated [Option] module emits boilerplate that callers may not touch:

     - -32 silences the templated heap/stack copies of [sexp_of_t]/[sexp_of_value]
     - -34 silences [type _uopt_empty = |] and the [type nonrec value = t] aliases when
       the [Option] module is unused
     - -60 silences the [%optional]-syntax wrapping when no caller uses [%optional] *)
  pstr_attribute
    ~loc
    (attribute
       ~loc
       ~name:{ txt = "ocaml.warning"; loc }
       ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-32-34-60") [] ]))
;;

(* The empty variant [type _uopt_empty = |] exists solely to drive the refutation pattern
   in [raise_match] below: a value of type [_uopt_empty] is uninhabited, so a
   [match _ : _uopt_empty with | _ -> .] branch is statically known to be unreachable. The
   match is wrapped around [Stdlib.raise ...] purely as a syntactic vehicle to produce an
   expression with a polymorphic result type. *)
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

(* The body of [value_exn] in the [none] branch. Raises [Invalid_argument] but uses a
   refutation pattern (matching [_ : _uopt_empty] with [.]) so the expression has a fully
   polymorphic result type ['a]. This lets [value_exn] return [value] in any branch
   without OCaml inferring a concrete type from the [raise]. *)
let raise_match ~loc ~type_name =
  let msg = estring ~loc (Printf.sprintf "%s.Option.value_exn: none" type_name) in
  let nothing_ty = ptyp_constr ~loc { txt = Lident "_uopt_empty"; loc } [] in
  pexp_match
    ~loc
    [%expr Stdlib.raise (Stdlib.Invalid_argument [%e msg])]
    [ { pc_lhs = ppat_constraint ~loc (ppat_var ~loc { txt = "_"; loc }) nothing_ty
      ; pc_guard = None
      ; pc_rhs = pexp_unreachable ~loc
      }
    ]
;;

(* The doubly-nested [Optional_syntax.Optional_syntax] is the lookup convention required
   by Jane Street's [%optional] syntax extension: [%optional T.x] expects to find
   [T.Option.Optional_syntax.Optional_syntax.{is_none, unsafe_value}]. *)
let optional_syntax_mod ~loc ~assume_zero_alloc =
  let app_t name = eapply ~loc (evar ~loc name) [ evar ~loc "t" ] in
  wrap_in_module_str
    ~loc
    "Optional_syntax"
    [ wrap_in_module_str
        ~loc
        "Optional_syntax"
        [ let_inline_t ~loc ~assume_zero_alloc "is_none" (app_t "is_none")
        ; let_inline_t ~loc "unsafe_value" (app_t "unsafe_value")
        ]
    ]
;;

(* Mode selection: an explicit [none = ...] override switches to the sentinel
   representation (Option.t = value, [Option.none] is the user's reserved value);
   otherwise we fall back to the tagged representation [#(bool * value)]. *)
let repr_kind_of_options ~none_override =
  if Option.is_some none_override then Sentinel_repr else Tagged_repr
;;

(* ===== signature ===== *)

let gen_sig_items ~loc ~type_name ~t_typ =
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
  let bool_typ = [%type: bool] in
  let sexp_typ =
    ptyp_constr ~loc { txt = Ldot (Ldot (Lident "Sexplib0", "Sexp"), "t"); loc } []
  in
  let arrow a b = ptyp_arrow ~loc Nolabel a b in
  let mk_val name ty =
    psig_value ~loc (value_description ~loc ~name:{ txt = name; loc } ~type_:ty ~prim:[])
  in
  [ abstract_sig_type ~loc "value" (ptyp_constr ~loc { txt = Lident type_name; loc } [])
  ; abstract_sig_type ~loc "t" t_typ
  ; mk_val "none" t_typ
  ; mk_val "some" (arrow value_typ t_typ)
  ; mk_val "is_none" (arrow t_typ bool_typ)
  ; mk_val "is_some" (arrow t_typ bool_typ)
  ; mk_val
      "value"
      (arrow t_typ (ptyp_arrow ~loc (Labelled "default") value_typ value_typ))
  ; mk_val "value_exn" (arrow t_typ value_typ)
  ; mk_val "unsafe_value" (arrow t_typ value_typ)
  ; templated_heap_stack_sig_value
      ~loc
      ~name:"sexp_of_value"
      ~arg_type:value_typ
      ~result_type:sexp_typ
  ; templated_heap_stack_sig_value
      ~loc
      ~name:"sexp_of_t"
      ~arg_type:t_typ
      ~result_type:sexp_typ
  ; wrap_in_module_sig
      ~loc
      "Optional_syntax"
      [ wrap_in_module_sig
          ~loc
          "Optional_syntax"
          [ mk_val "is_none" (arrow t_typ bool_typ)
          ; mk_val "unsafe_value" (arrow t_typ value_typ)
          ]
      ]
  ]
;;

(* ===== payload-shaped helpers ===== *)

let default_payload_expr ~loc = function
  | Scalar kind -> Scalar_gen.default_payload_expr ~loc kind
  | Unboxed_record labels ->
    let fields =
      List.map
        (fun (ld : label_declaration) ->
          let name = ld.pld_name.txt in
          let payload =
            match classify_record_field ~loc ld with
            | Record_field_scalar kind -> Scalar_gen.default_payload_expr ~loc kind
            | Record_field_contract _ -> evar ~loc (Record_gen.contract_payload_name name)
            | Record_field_immediate imm -> Record_gen.immediate_default_expr ~loc imm
            | Record_field_opaque field_type ->
              opaque_default_payload_expr ~loc field_type
          in
          { txt = Lident name; loc }, payload)
        labels
    in
    pexp_record_unboxed_product ~loc fields None
;;

let sentinel_none_expr ~loc ~type_info ~none_override =
  match type_info with
  | Scalar kind -> Scalar_gen.none_expr ~loc ~kind ~none_override
  | Unboxed_record labels -> Record_gen.gen_unboxed_record_none ~loc labels ~none_override
;;

let sentinel_is_none_body ~loc ~type_info ~none_override t_expr =
  match type_info with
  | Scalar kind -> Scalar_gen.is_none_body ~loc ~kind ~none_override t_expr
  | Unboxed_record labels ->
    Record_gen.gen_unboxed_record_is_none_sentinel ~loc labels ~none_override t_expr
;;

let helper_items ~loc = function
  | Scalar kind -> Scalar_gen.helper_items ~loc kind
  | Unboxed_record labels ->
    let scalar_kinds =
      labels
      |> List.filter_map (fun ld ->
        match classify_record_field ~loc ld with
        | Record_field_scalar kind -> Some kind
        | Record_field_contract _ | Record_field_immediate _ | Record_field_opaque _ ->
          None)
      |> List.sort_uniq compare_scalar_kind
    in
    List.concat_map (Scalar_gen.helper_items ~loc) scalar_kinds
;;

let contract_items ~loc ~repr_kind = function
  | Scalar _ -> []
  | Unboxed_record labels ->
    let need_is_none =
      match repr_kind with
      | Sentinel_repr -> true
      | Tagged_repr -> false
    in
    Record_gen.contract_helper_items ~loc labels ~need_is_none
;;

let sexp_of_value_body ~loc = function
  | Scalar kind -> Scalar_gen.sexp_of_value_expr ~loc kind (evar ~loc "v")
  | Unboxed_record labels ->
    let field_sexps =
      List.map
        (fun (ld : label_declaration) ->
          let name = ld.pld_name.txt in
          let field_access =
            pexp_unboxed_field ~loc (evar ~loc "v") { txt = Lident name; loc }
          in
          let field_sexp =
            match classify_record_field ~loc ld with
            | Record_field_scalar kind ->
              Scalar_gen.sexp_of_value_expr ~loc kind field_access
            | Record_field_contract base ->
              eapply
                ~loc
                (with_alloc_var
                   ~loc
                   (eqident_lid ~loc base [ "Option"; "sexp_of_value" ]))
                [ field_access ]
            | Record_field_immediate Imm_int ->
              [%expr Sexplib0.Sexp.Atom (Stdlib.string_of_int [%e field_access])]
            | Record_field_immediate Imm_bool ->
              [%expr Sexplib0.Sexp.Atom (Stdlib.string_of_bool [%e field_access])]
            | Record_field_immediate Imm_char ->
              [%expr Sexplib0.Sexp.Atom (Stdlib.String.make 1 [%e field_access])]
            | Record_field_immediate Imm_float ->
              [%expr Sexplib0.Sexp.Atom (Stdlib.Float.to_string [%e field_access])]
            | Record_field_opaque _ ->
              (* No type-aware sexp converter is available. Touch the field to keep the
                 expression well-typed, then emit an opaque placeholder. *)
              [%expr
                let _ = [%e field_access] in
                Sexplib0.Sexp.Atom "<opaque>"]
          in
          [%expr
            Sexplib0.Sexp.List
              [ Sexplib0.Sexp.Atom [%e estring ~loc name]; [%e field_sexp] ]])
        labels
    in
    let list_expr =
      List.fold_right
        (fun elem acc -> [%expr [%e elem] :: [%e acc]])
        field_sexps
        [%expr []]
    in
    [%expr Sexplib0.Sexp.List [%e list_expr]]
;;

let sexp_of_t_body ~loc =
  [%expr
    if is_none v
    then Sexplib0.Sexp.List []
    else
      Sexplib0.Sexp.List
        [ [%e
            eapply
              ~loc
              (with_alloc_var ~loc (evar ~loc "sexp_of_value"))
              [ [%expr unsafe_value v] ]]
        ]]
;;

(* ===== option module body for a payload type ===== *)

let gen_str_option ~loc ~type_name ~type_info ~none_override =
  let repr_kind = repr_kind_of_options ~none_override in
  (* When sentinel-mode [is_none] falls back to polymorphic equality on an opaque field,
     the body lowers to [caml_equal] which the static [@@zero_alloc] checker
     conservatively treats as potentially-allocating. Use [@@zero_alloc assume] on
     [is_none] and the transitive callers ([is_some], [value], [value_exn], and
     [Optional_syntax.is_none]) so callers continue to see them as zero-alloc. *)
  let assume_zero_alloc =
    match repr_kind, type_info with
    | Sentinel_repr, Unboxed_record labels ->
      Record_gen.unboxed_record_is_none_uses_poly_eq ~loc labels ~none_override
    | _ -> false
  in
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
  let t_manifest =
    match repr_kind with
    | Sentinel_repr -> value_typ
    | Tagged_repr -> tagged_option_type ~loc
  in
  let none_body =
    match repr_kind with
    | Sentinel_repr -> sentinel_none_expr ~loc ~type_info ~none_override
    | Tagged_repr -> tagged_none_expr ~loc (default_payload_expr ~loc type_info)
  in
  let some_body =
    fun_one
      ~loc
      "v"
      (match repr_kind with
       | Sentinel_repr -> evar ~loc "v"
       | Tagged_repr -> tagged_some_expr ~loc (evar ~loc "v"))
  in
  let is_none_body =
    match repr_kind with
    | Sentinel_repr ->
      sentinel_is_none_body ~loc ~type_info ~none_override (evar ~loc "t")
    | Tagged_repr ->
      [%expr
        match t with
        | #(is_some, _) -> not is_some]
  in
  let is_some_body =
    match repr_kind with
    | Sentinel_repr -> enot ~loc (eapply ~loc (evar ~loc "is_none") [ evar ~loc "t" ])
    | Tagged_repr ->
      [%expr
        match t with
        | #(is_some, _) -> is_some]
  in
  let value_body =
    match repr_kind with
    | Sentinel_repr -> [%expr if is_none t then default else t]
    | Tagged_repr ->
      [%expr
        match t with
        | #(is_some, value) -> if is_some then value else default]
  in
  let raise_match = raise_match ~loc ~type_name in
  let value_exn_body =
    match repr_kind with
    | Sentinel_repr -> [%expr if is_none t then [%e raise_match] else t]
    | Tagged_repr ->
      [%expr
        match t with
        | #(is_some, value) -> if is_some then value else [%e raise_match]]
  in
  let unsafe_value_body =
    match repr_kind with
    | Sentinel_repr -> evar ~loc "t"
    | Tagged_repr ->
      [%expr
        match t with
        | #(_, value) -> value]
  in
  [ suppress_warnings ~loc
  ; abstract_type ~loc "value" (ptyp_constr ~loc { txt = Lident type_name; loc } [])
  ; abstract_type ~loc "t" t_manifest
  ]
  @ helper_items ~loc type_info
  @ contract_items ~loc ~repr_kind type_info
  @ [ let_def ~loc "none" none_body
    ; let_inline ~loc "some" some_body
    ; let_inline_t ~loc ~assume_zero_alloc "is_none" is_none_body
    ; let_inline_t ~loc ~assume_zero_alloc "is_some" is_some_body
    ; let_inline ~loc ~assume_zero_alloc "value" (fun_t_default ~loc value_body)
    ; (* [type_empty] must precede [value_exn] so [_uopt_empty] is in scope. *)
      type_empty ~loc
    ; let_inline_t ~loc ~assume_zero_alloc "value_exn" value_exn_body
    ; let_inline_t ~loc "unsafe_value" unsafe_value_body
    ; let_template
        ~loc
        ~name:"sexp_of_value"
        ~arg:"v"
        ~body:(sexp_of_value_body ~loc type_info)
    ; let_template ~loc ~name:"sexp_of_t" ~arg:"v" ~body:(sexp_of_t_body ~loc)
    ; optional_syntax_mod ~loc ~assume_zero_alloc
    ]
;;

let gen_sig_option ~loc ~type_name ~none_override =
  let value_typ = ptyp_constr ~loc { txt = Lident "value"; loc } [] in
  let t_typ =
    match repr_kind_of_options ~none_override with
    | Sentinel_repr -> value_typ
    | Tagged_repr -> tagged_option_type ~loc
  in
  gen_sig_items ~loc ~type_name ~t_typ
;;

(* ===== alias of an existing M with [@@deriving unboxed_option] ===== *)

let gen_str_alias ~loc ~type_name ~base =
  let m_option parts = eqident_lid ~loc (Ldot (base, "Option")) parts in
  (* [let foo t = M.Option.foo t], for the eta-expanded delegating bindings below. *)
  let delegate_t name =
    let_inline_t ~loc name (eapply ~loc (m_option [ name ]) [ evar ~loc "t" ])
  in
  let template_via name arg =
    let_template
      ~loc
      ~name
      ~arg
      ~body:(eapply ~loc (with_alloc_var ~loc (m_option [ name ])) [ evar ~loc arg ])
  in
  let raise_match = raise_match ~loc ~type_name in
  [ suppress_warnings ~loc
  ; abstract_type ~loc "value" (ptyp_constr ~loc { txt = Lident type_name; loc } [])
  ; abstract_type
      ~loc
      "t"
      (ptyp_constr ~loc { txt = Ldot (Ldot (base, "Option"), "t"); loc } [])
  ; let_def ~loc "none" (m_option [ "none" ])
  ; let_inline
      ~loc
      "some"
      (fun_one ~loc "v" (eapply ~loc (m_option [ "some" ]) [ evar ~loc "v" ]))
  ; delegate_t "is_none"
  ; delegate_t "is_some"
  ; let_inline
      ~loc
      "value"
      (fun_t_default
         ~loc
         (pexp_apply
            ~loc
            (m_option [ "value" ])
            [ Nolabel, evar ~loc "t"; Labelled "default", evar ~loc "default" ]))
  ; delegate_t "unsafe_value"
  ; type_empty ~loc
  ; let_inline_t
      ~loc
      "value_exn"
      [%expr if is_none t then [%e raise_match] else unsafe_value t]
  ; template_via "sexp_of_value" "v"
  ; template_via "sexp_of_t" "t"
  ; optional_syntax_mod ~loc ~assume_zero_alloc:false
  ]
;;

let gen_sig_alias ~loc ~type_name ~base =
  let t_typ = ptyp_constr ~loc { txt = Ldot (Ldot (base, "Option"), "t"); loc } [] in
  gen_sig_items ~loc ~type_name ~t_typ
;;

(* ===== plumbing ===== *)

let make_args () =
  let open Deriving.Args in
  empty +> arg "none" Ast_pattern.__
;;

let reject_none_on_alias ~loc none_opt =
  if Option.is_some none_opt
  then
    Location.raise_errorf
      ~loc
      "ppx_uopt: 'none' is not supported on type aliases; the alias delegates to the \
       base module's [Option] entirely."
;;

let str_type_decl =
  Deriving.Generator.V2.make (make_args ()) (fun ~ctxt (_rec_flag, tds) none_opt ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match tds with
    | [ td ] ->
      let type_name = td.ptype_name.txt in
      let str_items, sig_items =
        match detect_type_info ~loc td with
        | Alias base ->
          reject_none_on_alias ~loc none_opt;
          gen_str_alias ~loc ~type_name ~base, gen_sig_alias ~loc ~type_name ~base
        | Payload pti ->
          ( gen_str_option ~loc ~type_name ~type_info:pti ~none_override:none_opt
          , gen_sig_option ~loc ~type_name ~none_override:none_opt )
      in
      [ wrap_in_constrained_module_str ~loc "Option" ~sig_items ~str_items ]
    | _ ->
      Location.raise_errorf ~loc "ppx_uopt: only single type declarations are supported")
;;

let sig_type_decl =
  Deriving.Generator.V2.make (make_args ()) (fun ~ctxt (_rec_flag, tds) none_opt ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match tds with
    | [ td ] ->
      let type_name = td.ptype_name.txt in
      let items =
        match detect_sig_info ~loc td with
        | `Alias base ->
          reject_none_on_alias ~loc none_opt;
          gen_sig_alias ~loc ~type_name ~base
        | `Payload -> gen_sig_option ~loc ~type_name ~none_override:none_opt
      in
      [ wrap_in_module_sig ~loc "Option" items ]
    | _ ->
      Location.raise_errorf ~loc "ppx_uopt: only single type declarations are supported")
;;

let () = Deriving.add "unboxed_option" ~str_type_decl ~sig_type_decl |> Deriving.ignore
