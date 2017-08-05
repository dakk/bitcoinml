type t

val addr_of_pub			: int -> bytes -> string
val addr_of_pubhash	: int -> bytes -> string

val priv_to_wif			: bytes -> string
val wif_to_priv			: string -> bytes

val from_wif        : string -> t option
val from_priv				: bytes -> t option
val to_wif					: t -> string
val to_address			: t -> string

val verify					: bytes -> bytes -> bytes -> bool
