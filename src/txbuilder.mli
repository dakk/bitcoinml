type t = {
	mutable tx  : Tx.t;
}

val from_tx : Tx.t -> t

val add_input   : t -> Hash.t -> int -> int -> Script.t -> t
val add_output  : t -> bytes -> int -> t

val sign    : t -> Keypair.t -> t
val to_tx   : t -> Tx.t