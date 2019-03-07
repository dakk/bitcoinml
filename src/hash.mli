type t = string
type hash = t
type b = string

val reverse				: string -> string

val to_bin				: t -> b
val to_bin_norev	: t -> b
val to_bigint     : t -> Big_int.big_int

val of_bin				: b -> t
val of_bin_norev  : b -> t
val zero					: t



val sha1		  	: t -> t
val sha256			: t -> t
val ripemd160		: t -> t

val hash160			: t -> t
val hash256			: t -> t
val dsha256			: t -> t
val checksum4 	    : t -> t
