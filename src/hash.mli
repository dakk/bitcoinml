type t = string [@@deriving sexp]
type hash = t [@@deriving sexp]
type b = bytes [@@deriving sexp]

val reverse				: string -> string

val to_bin				: t -> b
val to_bigint     : t -> Big_int.big_int

val of_bin				: b -> t
val of_bin_norev  : b -> t
val zero					: unit -> t

val to_string     : bytes -> string


val sha1		  	: bytes -> bytes
val sha256			: bytes -> bytes
val ripemd160		: bytes -> bytes

val hash160			: bytes -> bytes
val hash256			: bytes -> bytes
val dsha256			: bytes -> bytes
val checksum4 	: bytes -> bytes
