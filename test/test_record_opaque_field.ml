(* Tagged-mode unboxed_option on a record whose fields aren't all recognised
   unboxed scalars or [M.t] contract types: plain immediate [int] and
   value-layout [string] fall through to the opaque-field path, where the
   deriver materialises a placeholder via [(Stdlib.Obj.magic 0 : <type>)] for
   the never-observed [Option.none] payload.

   This is the whole point of tagged mode - the payload behind [Option.none]
   is never inspected by [is_none] (which only checks the leading bool tag),
   so any well-typed value works. *)

module Mixed = struct
  type t =
    #{ id : int64#
     ; face : int
     ; tag : string
     }
  [@@deriving unboxed_option]
end

let () =
  assert (Mixed.Option.is_none Mixed.Option.none);
  let v = Mixed.Option.some #{ Mixed.id = #42L; face = 3; tag = "hello" } in
  assert (Mixed.Option.is_some v);
  let payload = Mixed.Option.value_exn v in
  assert (Int64_u.equal payload.#id #42L);
  let face = payload.#face in
  let tag = payload.#tag in
  assert (face = 3);
  assert (String.equal tag "hello");
  (* Round-trip a value that happens to have the int64-default 0L: tagged
     representation never confuses real [some] values with [none]. *)
  let zeroish = Mixed.Option.some #{ Mixed.id = #0L; face = 0; tag = "" } in
  assert (Mixed.Option.is_some zeroish)
;;
