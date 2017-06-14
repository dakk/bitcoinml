(* Hash in letchain are the readable string representation *)
type t = string
type hash = t
type b = bytes

val reverse		    : string -> string

val to_bin			: t -> b
val to_bigint       : t -> Big_int.big_int

val of_bin			: b -> t
val of_bin_norev    : b -> t
val zero			: unit -> t

val print_bin       : bytes -> string