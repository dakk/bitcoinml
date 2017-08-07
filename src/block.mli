open Stdint

module Header : sig
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		time		: float;
		bits		: uint32;
		nonce		: uint32;
	}

	val parse 		: bytes -> t option
	val serialize	: t -> bytes
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		:	int;
}

val parse		: bytes -> t option
val serialize	: t -> bytes
val to_string	: t -> string