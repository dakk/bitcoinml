open Stdint
open Bitstring

val parse_varint : Bitstring.t -> Uint64.t * Bitstring.t
val bitstring_of_varint : Int64.t -> Bitstring.t