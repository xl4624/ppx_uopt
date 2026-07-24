open Ppxlib
open Ast_builder.Default
open Uopt_types
open Ast_helpers
open Classification

(* The function bodies an [Option] module needs, parameterised over the choice of
   representation (sentinel vs tagged). Each field after [t_manifest]/[none] is a complete
   function expression so the emission step is uniform. *)
type repr_bodies =
  { t_manifest : core_type
  ; none : expression (* a value of type [t] *)
  ; some : expression (* [fun v -> ...] *)
  ; is_none : expression (* [fun t -> ...] *)
  ; is_some : expression (* [fun t -> ...] *)
  ; value : expression (* [fun t ~default -> ...] *)
  ; value_exn : expression (* [fun t -> ...] *)
  ; unsafe_value : expression (* [fun t -> ...] *)
  }

let abstract_type_decl ~loc name manifest =
  type_declaration
    ~loc
    ~name:(Located.mk ~loc name)
    ~params:[]
    ~cstrs:[]
    ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some manifest)
;;

let abstract_type ~loc name manifest =
  pstr_type ~loc Nonrecursive [ abstract_type_decl ~loc name manifest ]
;;

let abstract_sig_type ~loc name manifest =
  psig_type ~loc Nonrecursive [ abstract_type_decl ~loc name manifest ]
;;

let pstr_module_binding ~loc name expr =
  pstr_module ~loc (module_binding ~loc ~name:(Located.mk ~loc (Some name)) ~expr)
;;

let wrap_in_module_str ~loc name items =
  pstr_module_binding ~loc name (pmod_structure ~loc items)
;;

let wrap_in_constrained_module_str ~loc name ~sig_items ~str_items =
  pstr_module_binding
    ~loc
    name
    (pmod_constraint ~loc (pmod_structure ~loc str_items) (pmty_signature ~loc sig_items))
;;

let wrap_in_module_sig ~loc name items =
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:(Located.mk ~loc (Some name))
       ~type_:(pmty_signature ~loc items))
;;

let let_template ~loc ~name ~arg ~body =
  let vb =
    { (value_binding
         ~loc
         ~pat:(pvar ~loc name)
         ~expr:(eabstract ~loc [ pvar ~loc arg ] (with_exclave_if_stack ~loc body)))
      with
      pvb_attributes = [ alloc_heap_stack_attr ~loc; zero_alloc_ignore_attr ~loc ]
    }
  in
  pstr_template ~loc (pstr_value ~loc Nonrecursive [ vb ])
;;

(* The generated [Option] module emits boilerplate that callers may not touch:

   - -32 silences the templated heap/stack copies of [sexp_of_t]/[sexp_of_value]
   - -34 silences [type _uopt_empty = |] and the [type nonrec value = t] aliases when the
     [Option] module is unused
   - -60 silences the [%optional]-syntax wrapping when no caller uses [%optional] *)
let suppress_warnings ~loc = [%stri [@@@ocaml.warning "-32-34-60"]]

(* [type _uopt_empty = |] is uninhabited; a [(_ : _uopt_empty)] refutation case in
   [raise_match] gives [Stdlib.raise ...] a fully polymorphic result type. Metaquot can't
   parse [type X = |], so the declaration is built explicitly. *)
let type_empty ~loc =
  pstr_type
    ~loc
    Nonrecursive
    [ type_declaration
        ~loc
        ~name:(Located.mk ~loc "_uopt_empty")
        ~params:[]
        ~cstrs:[]
        ~kind:(Ptype_variant [])
        ~private_:Public
        ~manifest:None
    ]
;;

let raise_match ~loc ~type_name =
  let msg = estring ~loc (Printf.sprintf "%s.Option.value_exn: none" type_name) in
  [%expr
    match Stdlib.raise (Stdlib.Invalid_argument [%e msg]) with
    | (_ : _uopt_empty) -> .]
;;

(* The doubly-nested [Optional_syntax.Optional_syntax] is the lookup convention required
   by Jane Street's [%optional] syntax extension: [%optional T.x] expects to find
   [T.Option.Optional_syntax.Optional_syntax.{is_none, unsafe_value}]. *)
let optional_syntax_mod ~loc ~assume_zero_alloc =
  let delegate name = [%expr fun t -> [%e evar ~loc name] t] in
  wrap_in_module_str
    ~loc
    "Optional_syntax"
    [ wrap_in_module_str
        ~loc
        "Optional_syntax"
        [ let_inline ~loc ~assume_zero_alloc "is_none" (delegate "is_none")
        ; let_inline ~loc "unsafe_value" (delegate "unsafe_value")
        ]
    ]
;;

let repr_kind_of_options ~none_override =
  if Option.is_some none_override then Sentinel_repr else Tagged_repr
;;

let gen_sig_items ~loc ~type_name ~t_typ =
  let sexp_typ = [%type: Sexplib0.Sexp.t] in
  let mk_val name ty =
    psig_value
      ~loc
      (value_description ~loc ~name:(Located.mk ~loc name) ~type_:ty ~prim:[])
  in
  let sexp_of name arg_type =
    templated_heap_stack_sig_value ~loc ~name ~arg_type ~result_type:sexp_typ
  in
  [ abstract_sig_type ~loc "value" (ptyp_constr ~loc (Located.lident ~loc type_name) [])
  ; abstract_sig_type ~loc "t" t_typ
  ; mk_val "none" t_typ
  ; mk_val "some" [%type: value -> [%t t_typ]]
  ; mk_val "is_none" [%type: [%t t_typ] -> bool]
  ; mk_val "is_some" [%type: [%t t_typ] -> bool]
  ; mk_val "value" [%type: [%t t_typ] -> default:value -> value]
  ; mk_val "value_exn" [%type: [%t t_typ] -> value]
  ; mk_val "unsafe_value" [%type: [%t t_typ] -> value]
  ; sexp_of "sexp_of_value" [%type: value]
  ; sexp_of "sexp_of_t" t_typ
  ; wrap_in_module_sig
      ~loc
      "Optional_syntax"
      [ wrap_in_module_sig
          ~loc
          "Optional_syntax"
          [ mk_val "is_none" [%type: [%t t_typ] -> bool]
          ; mk_val "unsafe_value" [%type: [%t t_typ] -> value]
          ]
      ]
  ]
;;

let default_payload_for_field ~loc (ld : label_declaration) =
  match classify_record_field ~loc ld with
  | Record_field_scalar kind -> Scalar_gen.default_payload_expr ~loc kind
  | Record_field_contract _ ->
    evar ~loc (Record_gen.contract_payload_name ld.pld_name.txt)
  | Record_field_immediate imm -> Record_gen.immediate_default_expr ~loc imm
  | Record_field_opaque field_type -> opaque_default_payload_expr ~loc field_type
;;

let default_payload_expr ~loc = function
  | Scalar kind -> Scalar_gen.default_payload_expr ~loc kind
  | Unboxed_tuple kinds ->
    pexp_unboxed_tuple
      ~loc
      (List.map (fun kind -> None, Scalar_gen.default_payload_expr ~loc kind) kinds)
  | Unboxed_record labels ->
    let fields =
      List.map
        (fun (ld : label_declaration) ->
          Located.lident ~loc ld.pld_name.txt, default_payload_for_field ~loc ld)
        labels
    in
    pexp_record_unboxed_product ~loc fields None
;;

let sentinel_none_expr ~loc ~type_info ~none_override =
  match type_info with
  | Scalar kind -> Scalar_gen.none_expr ~loc ~kind ~none_override
  | Unboxed_tuple kinds -> Tuple_gen.gen_unboxed_tuple_none ~loc kinds ~none_override
  | Unboxed_record labels -> Record_gen.gen_unboxed_record_none ~loc labels ~none_override
;;

let sentinel_is_none_body ~loc ~type_info ~none_override t_expr =
  match type_info with
  | Scalar kind -> Scalar_gen.is_none_body ~loc ~kind ~none_override t_expr
  | Unboxed_tuple kinds ->
    Tuple_gen.gen_unboxed_tuple_is_none ~loc kinds ~none_override t_expr
  | Unboxed_record labels ->
    Record_gen.gen_unboxed_record_is_none_sentinel ~loc labels ~none_override t_expr
;;

let helper_items ~loc info =
  let kinds =
    match info with
    | Scalar kind -> [ kind ]
    | Unboxed_tuple kinds -> kinds
    | Unboxed_record labels ->
      labels
      |> List.filter_map (fun ld ->
        match classify_record_field ~loc ld with
        | Record_field_scalar kind -> Some kind
        | Record_field_contract _ | Record_field_immediate _ | Record_field_opaque _ ->
          None)
      |> List.sort_uniq compare_scalar_kind
  in
  let primitives =
    kinds
    |> List.concat_map Scalar_gen.primitives_for_kind
    |> List.sort_uniq Scalar_gen.compare_primitive
  in
  List.map (Scalar_gen.primitive_item ~loc) primitives
  @ List.concat_map (Scalar_gen.extra_bindings ~loc) kinds
;;

let contract_items ~loc ~repr_kind = function
  | Scalar _ | Unboxed_tuple _ -> []
  | Unboxed_record labels ->
    let need_is_none =
      match repr_kind with
      | Sentinel_repr -> true
      | Tagged_repr -> false
    in
    Record_gen.contract_helper_items ~loc labels ~need_is_none
;;

let sexp_of_field ~loc ld field_access =
  match classify_record_field ~loc ld with
  | Record_field_scalar kind -> Scalar_gen.sexp_of_value_expr ~loc kind field_access
  | Record_field_contract base ->
    let sexp_of_value =
      with_alloc_var ~loc (qual_evar ~loc base [ "Option"; "sexp_of_value" ])
    in
    [%expr [%e sexp_of_value] [%e field_access]]
  | Record_field_immediate imm ->
    let str =
      match imm with
      | Imm_int -> [%expr Stdlib.string_of_int [%e field_access]]
      | Imm_bool -> [%expr Stdlib.string_of_bool [%e field_access]]
      | Imm_char -> [%expr Stdlib.String.make 1 [%e field_access]]
      | Imm_float -> [%expr Stdlib.Float.to_string [%e field_access]]
    in
    [%expr Sexplib0.Sexp.Atom [%e str]]
  | Record_field_opaque _ ->
    (* No type-aware sexp converter is available; touch the field to keep it well-typed. *)
    [%expr
      let _ = [%e field_access] in
      Sexplib0.Sexp.Atom "<opaque>"]
;;

let sexp_of_value_body ~loc = function
  | Scalar kind -> Scalar_gen.sexp_of_value_expr ~loc kind (evar ~loc "v")
  | Unboxed_tuple kinds ->
    Tuple_gen.gen_unboxed_tuple_sexp_of_value ~loc kinds (evar ~loc "v")
  | Unboxed_record labels ->
    let field_sexps =
      List.map
        (fun (ld : label_declaration) ->
          let name = ld.pld_name.txt in
          let access =
            pexp_unboxed_field ~loc (evar ~loc "v") (Located.lident ~loc name)
          in
          [%expr
            Sexplib0.Sexp.List
              [ Sexplib0.Sexp.Atom [%e estring ~loc name]
              ; [%e sexp_of_field ~loc ld access]
              ]])
        labels
    in
    [%expr Sexplib0.Sexp.List [%e elist ~loc field_sexps]]
;;

let sexp_of_t_body ~loc =
  let sexp_of_value = with_alloc_var ~loc (evar ~loc "sexp_of_value") in
  [%expr
    if is_none v
    then Sexplib0.Sexp.List []
    else Sexplib0.Sexp.List [ [%e sexp_of_value] (unsafe_value v) ]]
;;

let gen_str_option ~loc ~type_name ~type_info ~none_override =
  let repr_kind = repr_kind_of_options ~none_override in
  (* [caml_equal] (from a poly-eq opaque-field discriminator) defeats the static
     [@@zero_alloc] checker, so those cases fall back to [@@zero_alloc assume]. *)
  let assume_zero_alloc =
    match repr_kind, type_info with
    | Sentinel_repr, Unboxed_record labels ->
      Record_gen.unboxed_record_is_none_uses_poly_eq ~loc labels ~none_override
    | Sentinel_repr, (Scalar _ | Unboxed_tuple _)
    | Tagged_repr, (Scalar _ | Unboxed_tuple _ | Unboxed_record _) -> false
  in
  let raise_match = raise_match ~loc ~type_name in
  let b =
    match repr_kind with
    | Sentinel_repr ->
      { t_manifest = [%type: value]
      ; none = sentinel_none_expr ~loc ~type_info ~none_override
      ; some = [%expr fun v -> v]
      ; is_none =
          [%expr
            fun t ->
              [%e sentinel_is_none_body ~loc ~type_info ~none_override (evar ~loc "t")]]
      ; is_some = [%expr fun t -> not (is_none t)]
      ; value = [%expr fun t ~default -> if is_none t then default else t]
      ; value_exn = [%expr fun t -> if is_none t then [%e raise_match] else t]
      ; unsafe_value = [%expr fun t -> t]
      }
    | Tagged_repr ->
      { t_manifest = tagged_option_type ~loc
      ; none = [%expr #(false, [%e default_payload_expr ~loc type_info])]
      ; some = [%expr fun v -> #(true, v)]
      ; is_none =
          [%expr
            fun t ->
              match t with
              | #(is_some, _) -> not is_some]
      ; is_some =
          [%expr
            fun t ->
              match t with
              | #(is_some, _) -> is_some]
      ; value =
          [%expr
            fun t ~default ->
              match t with
              | #(is_some, value) -> if is_some then value else default]
      ; value_exn =
          [%expr
            fun t ->
              match t with
              | #(is_some, value) -> if is_some then value else [%e raise_match]]
      ; unsafe_value =
          [%expr
            fun t ->
              match t with
              | #(_, value) -> value]
      }
  in
  [ suppress_warnings ~loc
  ; abstract_type ~loc "value" (ptyp_constr ~loc (Located.lident ~loc type_name) [])
  ; abstract_type ~loc "t" b.t_manifest
  ; type_empty ~loc
  ]
  @ helper_items ~loc type_info
  @ contract_items ~loc ~repr_kind type_info
  @ [ let_def ~loc "none" b.none
    ; let_inline ~loc "some" b.some
    ; let_inline ~loc ~assume_zero_alloc "is_none" b.is_none
    ; let_inline ~loc ~assume_zero_alloc "is_some" b.is_some
    ; let_inline ~loc ~assume_zero_alloc "value" b.value
    ; let_inline ~loc ~assume_zero_alloc "value_exn" b.value_exn
    ; let_inline ~loc "unsafe_value" b.unsafe_value
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
  gen_sig_items
    ~loc
    ~type_name
    ~t_typ:
      (match repr_kind_of_options ~none_override with
       | Sentinel_repr -> [%type: value]
       | Tagged_repr -> tagged_option_type ~loc)
;;

let m_option_t ~loc base =
  ptyp_constr ~loc (Located.mk ~loc (Ldot (Ldot (base, "Option"), "t"))) []
;;

let gen_str_alias ~loc ~type_name ~base =
  let m_option name = qual_evar ~loc base [ "Option"; name ] in
  let delegate name = let_inline ~loc name [%expr fun t -> [%e m_option name] t] in
  let template_via name arg =
    let_template
      ~loc
      ~name
      ~arg
      ~body:[%expr [%e with_alloc_var ~loc (m_option name)] [%e evar ~loc arg]]
  in
  let raise_match = raise_match ~loc ~type_name in
  [ suppress_warnings ~loc
  ; abstract_type ~loc "value" (ptyp_constr ~loc (Located.lident ~loc type_name) [])
  ; abstract_type ~loc "t" (m_option_t ~loc base)
  ; type_empty ~loc
  ; let_def ~loc "none" (m_option "none")
  ; let_inline ~loc "some" [%expr fun v -> [%e m_option "some"] v]
  ; delegate "is_none"
  ; delegate "is_some"
  ; let_inline ~loc "value" [%expr fun t ~default -> [%e m_option "value"] t ~default]
  ; delegate "unsafe_value"
  ; let_inline
      ~loc
      "value_exn"
      [%expr fun t -> if is_none t then [%e raise_match] else unsafe_value t]
  ; template_via "sexp_of_value" "v"
  ; template_via "sexp_of_t" "t"
  ; optional_syntax_mod ~loc ~assume_zero_alloc:false
  ]
;;

let gen_sig_alias ~loc ~type_name ~base =
  gen_sig_items ~loc ~type_name ~t_typ:(m_option_t ~loc base)
;;

let args () = Deriving.Args.(empty +> arg "none" Ast_pattern.__)

let reject_none_on_alias ~loc none_opt =
  if Option.is_some none_opt
  then
    Location.raise_errorf
      ~loc
      "ppx_uopt: 'none' is not supported on type aliases; the alias delegates to the \
       base module's [Option] entirely."
;;

let with_single_td ~ctxt tds k =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ td ] -> k ~loc ~type_name:td.ptype_name.txt td
  | _ ->
    Location.raise_errorf ~loc "ppx_uopt: only single type declarations are supported"
;;

let str_type_decl =
  Deriving.Generator.V2.make (args ()) (fun ~ctxt (_rec_flag, tds) none_opt ->
    with_single_td ~ctxt tds (fun ~loc ~type_name td ->
      let str_items, sig_items =
        match detect_type_info ~loc td with
        | Alias base ->
          reject_none_on_alias ~loc none_opt;
          gen_str_alias ~loc ~type_name ~base, gen_sig_alias ~loc ~type_name ~base
        | Payload pti ->
          ( gen_str_option ~loc ~type_name ~type_info:pti ~none_override:none_opt
          , gen_sig_option ~loc ~type_name ~none_override:none_opt )
      in
      [ wrap_in_constrained_module_str ~loc "Option" ~sig_items ~str_items ]))
;;

let sig_type_decl =
  Deriving.Generator.V2.make (args ()) (fun ~ctxt (_rec_flag, tds) none_opt ->
    with_single_td ~ctxt tds (fun ~loc ~type_name td ->
      let items =
        match detect_sig_info ~loc td with
        | `Alias base ->
          reject_none_on_alias ~loc none_opt;
          gen_sig_alias ~loc ~type_name ~base
        | `Payload -> gen_sig_option ~loc ~type_name ~none_override:none_opt
      in
      [ wrap_in_module_sig ~loc "Option" items ]))
;;

let () = Deriving.add "unboxed_option" ~str_type_decl ~sig_type_decl |> Deriving.ignore
