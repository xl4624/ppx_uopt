open! Float_u

module Local_field = struct
  type t = float#

  module Option = struct
    let none = Float_u.nan ()
    let is_none t = Float_u.is_nan t
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
  assert (Mixed_contract_option.is_none #{ x = Float_u.nan (); y = #1.0 });
  assert (Mixed_contract_option.is_none #{ x = #1.0; y = Local_field.Option.none });
  let v = Mixed_contract_option.some #{ x = #1.0; y = #2.0 } in
  assert (Mixed_contract_option.is_some v);
  assert (Float_u.equal (Mixed_contract_option.value_exn v).#x #1.0);
  assert (Float_u.equal (Mixed_contract_option.value_exn v).#y #2.0)
;;
