open Sexplib
open Conv
open Stdint

val b2l : bytes -> int list

val bytes_of_sexp : Sexp.t -> bytes
val sexp_of_bytes : bytes -> Sexp.t

val sexp_of_uint32 : Uint32.t -> Sexp.t
val uint32_of_sexp : Sexp.t -> Uint32.t