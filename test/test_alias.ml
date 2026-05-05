open! Float_u

(* [@@deriving unboxed_option] on a type alias [type t = M.t] generates an [Option]
   submodule that re-exports M.Option's functions. *)

module Base = struct
  type t = float# [@@deriving unboxed_option]
end

module Aliased : sig
  type t = Base.t [@@deriving unboxed_option]
end = struct
  type t = Base.t [@@deriving unboxed_option]
end

let () =
  assert (Aliased.Option.is_none Aliased.Option.none);
  let v = Aliased.Option.some #1.5 in
  assert (Aliased.Option.is_some v);
  assert (Float_u.equal (Aliased.Option.value_exn v) #1.5);
  (* Aliased.Option.t is structurally identical to Base.Option.t (same tagged repr). *)
  let direct : Base.Option.t = Aliased.Option.some #2.5 in
  assert (Base.Option.is_some direct);
  assert (Float_u.equal (Base.Option.value_exn direct) #2.5)
;;
