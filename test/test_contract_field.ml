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

module Nested_contract = struct
  type t = #{ x : Local_field.t } [@@deriving unboxed_option]
end

let () =
  assert (Nested_contract.Option.is_none Nested_contract.Option.none);
  assert (
    Float_u.is_nan (Nested_contract.Option.unchecked_value Nested_contract.Option.none).#x);
  let v = Nested_contract.Option.some #{ Nested_contract.x = #2.5 } in
  assert (Nested_contract.Option.is_some v);
  assert (Float_u.equal (Nested_contract.Option.value_exn v).#x #2.5);
  assert (
    Float_u.equal
      (Nested_contract.Option.value
         Nested_contract.Option.none
         ~default:(Nested_contract.Option.value_exn v))
        .#x
      #2.5)
;;
