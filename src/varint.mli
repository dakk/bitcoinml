open Stdint

val parse_varint : Bitstring.t -> Uint64.t * Bitstring.t
val bitstring_of_varint : Int64.t -> Bitstring.t
val encoding_length : Uint64.t -> int