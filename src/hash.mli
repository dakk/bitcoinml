(** An hash is a 32B binary string, represented in human readable hex encoded 64B string *)

type t = string
type hash = t
type b = string

val reverse				: string -> string
(** [reverse s] reverses the string [s] *)

val to_bin				: t -> b
(** [to_bin h] transforms [h] in binary format *)

val to_bin_norev	: t -> b
(** [to_bin_norev h] transforms [h] in binary format without reversing *)

val to_bigint     : t -> Big_int.big_int
(** [to_bigint h] transforms [h] in a [Big_int] *)

val of_bin				: b -> t
(** [of_bin data] transforms [data] in an hash *)

val of_bin_norev  : b -> t
(** [of_bin_norev data] transforms [data] in an hash without reversing *)

val zero					: t
(** [zero] hash with only zeros *)



val sha1		  	: t -> t
(** [sha1 data] returns the sha1 of [data] *)

val sha256			: t -> t
(** [sha256 data] returns the sha2 of [data] *)

val ripemd160		: t -> t
(** [ripemd160 data] returns the ripedm160 of [data] *)

val hash160			: t -> t
(** [hash160 data] returns the hash160 of [data] *)

val hash256			: t -> t
(** [hash256 data] returns the hash256 of [data] *)

val dsha256			: t -> t
(** [dsha256 data] returns the double sha2 of [data] *)

val checksum4 	    : t -> t
(** [checksum4 data] returns the checksum4 of [data] *)
