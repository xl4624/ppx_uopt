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

type mixed_contract =
  #{ x : Float_u.t
   ; y : Local_field.t
   }
[@@deriving unboxed_option]

module Mixed_contract_option = Option

let () =
  assert (Mixed_contract_option.is_none Mixed_contract_option.none);
  let none_payload = Mixed_contract_option.unchecked_value Mixed_contract_option.none in
  assert (Float_u.is_nan none_payload.#x);
  assert (Float_u.is_nan none_payload.#y);
  let v = Mixed_contract_option.some #{ x = #1.0; y = #2.0 } in
  assert (Mixed_contract_option.is_some v);
  assert (Float_u.equal (Mixed_contract_option.value_exn v).#x #1.0);
  assert (Float_u.equal (Mixed_contract_option.value_exn v).#y #2.0)
;;
