(** A VarInt (variable integer) is a field used in transaction data to indicate the number 
    of upcoming fields, or the length of an upcoming field. *)
open Stdint

val parse_varint : Bitstring.t -> Uint64.t * Bitstring.t
(** [parse_varint bits] parse Bitstring [bits] returing the Uint64.t value and the rest 
    of the bitstring *)

val bitstring_of_varint : Int64.t -> Bitstring.t
(** [bitstring_of_varint num] encodes [num] to its bitstring varint representation *)

val encoding_length : Uint64.t -> int
(** [encoding_length num] returns the length of [num] if encoded as varint *)
