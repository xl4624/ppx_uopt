open! Float_u

module Local_field = struct
  type t = float#

  module Option = struct
    type value = t
    type t = #(bool * value)

    let none = #(false, Float_u.nan ())
    let some v = #(true, v)

    let is_none = function
      | #(is_some, _) -> not is_some
    ;;

    let unchecked_value = function
      | #(_, value) -> value
    ;;

    let%template[@alloc a @ m = (heap @ global, stack @ local)]
                [@inline]
                [@zero_alloc ignore] sexp_of_value
      v
      =
      (Float_u.sexp_of_t [@alloc a]) v [@exclave_if_stack a]
    ;;

    let sexp_of_t t =
      Sexplib0.Sexp_conv.sexp_of_option
        (fun x -> x)
        (if is_none t then None else Some (sexp_of_value (unchecked_value t)))
    ;;
  end
end

module Mixed_contract = struct
  type t =
    #{ x : Float_u.t
     ; y : Local_field.t
     }
  [@@deriving unboxed_option]
end

let () =
  assert (Mixed_contract.Option.is_none Mixed_contract.Option.none);
  let none_payload = Mixed_contract.Option.unchecked_value Mixed_contract.Option.none in
  assert (Float_u.is_nan none_payload.#x);
  assert (Float_u.is_nan none_payload.#y);
  let v = Mixed_contract.Option.some #{ Mixed_contract.x = #1.0; y = #2.0 } in
  assert (Mixed_contract.Option.is_some v);
  assert (Float_u.equal (Mixed_contract.Option.value_exn v).#x #1.0);
  assert (Float_u.equal (Mixed_contract.Option.value_exn v).#y #2.0)
;;
