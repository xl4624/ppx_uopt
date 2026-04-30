(* Sentinel-mode unboxed_option on a record whose fields aren't recognised
   unboxed scalars or [M.t] contract types. The user provides an explicit
   [none] override and the deriver compares opaque fields with polymorphic
   equality. Mirrors test_record_opaque_field.ml but uses sentinel mode. *)

module Mixed = struct
  type t =
    #{ id : int
     ; flag : bool
     ; tag : string
     }
  [@@deriving unboxed_option { none = #{ id = -1; flag = false; tag = "" } }]
end

let () =
  (* Option.t is the same as the value type in sentinel mode (no extra tag). *)
  assert (Mixed.Option.is_none Mixed.Option.none);
  assert (Mixed.Option.is_none #{ Mixed.id = -1; flag = false; tag = "" });
  (* Any field differing from the sentinel makes the value some. *)
  assert (not (Mixed.Option.is_none #{ Mixed.id = 0; flag = false; tag = "" }));
  assert (not (Mixed.Option.is_none #{ Mixed.id = -1; flag = true; tag = "" }));
  assert (not (Mixed.Option.is_none #{ Mixed.id = -1; flag = false; tag = "x" }));
  let v = Mixed.Option.some #{ Mixed.id = 42; flag = true; tag = "hello" } in
  assert (Mixed.Option.is_some v);
  let payload = Mixed.Option.value_exn v in
  assert (payload.#id = 42);
  assert (payload.#flag = true);
  assert (String.equal payload.#tag "hello")
;;
