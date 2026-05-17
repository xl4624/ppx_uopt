open Test_helpers
open! Float_u

(* Tagged-mode record with both int8# and char# fields. Both kinds would individually emit
   an [external _uopt_equal_int8 : ... = "%int8#_equal"] helper; the deriver must dedupe
   the shared primitive so the generated module declares it only once. *)
module R = struct
  type t =
    #{ a : int8#
     ; b : char#
     }
  [@@deriving unboxed_option]
end

let () =
  let n = R.Option.none in
  assert (R.Option.is_none n);
  let v = R.Option.some #{ R.a = #1s; b = #'x' } in
  assert (R.Option.is_some v);
  let r = R.Option.value_exn v in
  assert (eq_int8_u r.#a #1s);
  assert (eq_char_u r.#b #'x')
;;
