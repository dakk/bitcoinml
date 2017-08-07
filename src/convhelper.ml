open Sexplib;;
open Conv;;
open Core;;
open Stdint;;

let bytes_of_sexp b = string_of_sexp b;;
let sexp_of_bytes b = sexp_of_string b;;

let sexp_of_uint32 b = Conv.sexp_of_int32 (Uint32.to_int32 b);;
let uint32_of_sexp b = Uint32.of_int32 (Conv.int32_of_sexp b);;