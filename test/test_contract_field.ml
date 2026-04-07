open! Float_u

module Local_field = struct
  type t = float#

  module Option = struct
    type value = t
    type t = #(bool * value)

    let none = #(false, Float_u.nan ())

    let unchecked_value = function
      | #(_, value) -> value
    ;;
  end
end

type nested_contract = #{ x : Local_field.t } [@@deriving unboxed_option]

module Nested_contract_option = Option

let () =
  assert (Nested_contract_option.is_none Nested_contract_option.none);
  assert (
    Float_u.is_nan (Nested_contract_option.unchecked_value Nested_contract_option.none).#x);
  let v = Nested_contract_option.some #{ x = #2.5 } in
  assert (Nested_contract_option.is_some v);
  assert (Float_u.equal (Nested_contract_option.value_exn v).#x #2.5);
  assert (
    Float_u.equal
      (Nested_contract_option.value
         Nested_contract_option.none
         ~default:(Nested_contract_option.value_exn v))
        .#x
      #2.5)
;;
