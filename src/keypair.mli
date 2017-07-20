type t = {
	pub     : string;
	priv    : string;
	address : string;
};;

val addr_of_pk			: int -> bytes -> string
val addr_of_pkh			: int -> bytes -> string

val priv_to_wif			: bytes -> string
val wif_to_priv			: string -> bytes

val from_wif        : string -> t option
val from_priv				: bytes -> t option
val to_wif					: t -> string

val sign            : t -> bytes -> bytes
val verify          : t -> bytes -> bool

