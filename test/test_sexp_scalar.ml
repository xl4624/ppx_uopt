open! Float_u

module Float_opt = struct
  type t = float# [@@deriving unboxed_option]
end

module Int32_opt = struct
  type t = int32# [@@deriving unboxed_option { none = #0l }]
end

module Int64_opt = struct
  type t = int64# [@@deriving unboxed_option { sentinel = true }]
end

let sexp_to_string sexp = Sexplib0.Sexp.to_string sexp

let () =
  (* Float none *)
  let none_sexp = Float_opt.Option.sexp_of_t Float_opt.Option.none in
  assert (String.equal (sexp_to_string none_sexp) "()");
  (* Float some *)
  let some_sexp = Float_opt.Option.sexp_of_t (Float_opt.Option.some #3.5) in
  assert (String.equal (sexp_to_string some_sexp) "(3.5)");
  (* Int32 none *)
  let none_sexp = Int32_opt.Option.sexp_of_t Int32_opt.Option.none in
  assert (String.equal (sexp_to_string none_sexp) "()");
  (* Int32 some *)
  let some_sexp = Int32_opt.Option.sexp_of_t (Int32_opt.Option.some #42l) in
  assert (String.equal (sexp_to_string some_sexp) "(42)");
  (* Int64 sentinel default none *)
  let none_sexp = Int64_opt.Option.sexp_of_t Int64_opt.Option.none in
  assert (String.equal (sexp_to_string none_sexp) "()");
  (* Int64 sentinel default some *)
  let some_sexp = Int64_opt.Option.sexp_of_t (Int64_opt.Option.some #99L) in
  assert (String.equal (sexp_to_string some_sexp) "(99)")
;;
