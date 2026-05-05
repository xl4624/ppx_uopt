module Imm = struct
  type t =
    #{ id : int
     ; flag : bool
     }
  [@@deriving unboxed_option { none = #{ id = -1; flag = false } }]
end

let () =
  assert (Imm.Option.is_none Imm.Option.none);
  assert (Imm.Option.is_none #{ Imm.id = -1; flag = false });
  assert (not (Imm.Option.is_none #{ Imm.id = 0; flag = false }));
  assert (not (Imm.Option.is_none #{ Imm.id = -1; flag = true }))
;;
