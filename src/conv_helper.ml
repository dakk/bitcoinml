open Sexplib;;
open Stdint;;

let bytes_of_sexp b = Conv.string_of_sexp b;;
let sexp_of_bytes b = Conv.sexp_of_string b;;

let sexp_of_uint32 b = Conv.sexp_of_int32 (Uint32.to_int32 b);;
let uint32_of_sexp b = Uint32.of_int32 (Conv.int32_of_sexp b);;


let rec b2l t = match Bytes.length t with 
| 0 -> []
| n ->
  let ch = Bytes.get t 0 |> Char.code in
  ch :: (b2l @@ Bytes.sub t 1 (n-1))
;;