open! Float_u

module Local_field = struct
  type t = float#

  module Option = struct
    let none = Float_u.nan ()
    let is_none t = Float_u.is_nan t
  end
end

type nested_contract = #{ x : Local_field.t } [@@deriving unboxed_option]

module Nested_contract_option = Option

let () =
  assert (Nested_contract_option.is_none Nested_contract_option.none);
  assert (Nested_contract_option.is_none #{ x = Local_field.Option.none });
  let v = Nested_contract_option.some #{ x = #2.5 } in
  assert (Nested_contract_option.is_some v);
  assert (Float_u.equal (Nested_contract_option.value_exn v).#x #2.5);
  assert (
    Float_u.equal
      (Nested_contract_option.value Nested_contract_option.none ~default:v).#x
      #2.5)
;;
