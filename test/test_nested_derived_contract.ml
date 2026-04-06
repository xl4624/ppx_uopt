open! Float_u

module Scalar = struct
  type t = float# [@@deriving unboxed_option]
end

module Record = struct
  type t = #{ value : Scalar.t } [@@deriving unboxed_option]
end

let () =
  assert (Record.Option.is_none Record.Option.none);
  assert (Record.Option.is_none #{ Record.value = Scalar.Option.none });
  let v = Record.Option.some #{ Record.value = #6.25 } in
  assert (Record.Option.is_some v);
  assert (Float_u.equal (Record.Option.value_exn v).#value #6.25);
  assert (Float_u.equal (Record.Option.value Record.Option.none ~default:v).#value #6.25)
;;
