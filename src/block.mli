open Stdint

(** Block header parsing / serialization module *)
module Header : sig
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		time		: float;
		bits		: string;
		nonce		: uint32;
	}

	val parse 		: bytes -> t option
	(** Parse a block header *)

	val serialize	: t -> bytes
	(** Serialize a block header *)

	val check_target : t -> bool
	(** Check the nbits / hash target *)
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		:	int;
}


val parse					: bytes -> t option
(** Parse a block *)

val parse_legacy	: bytes -> t option
(** Parse a legacy block *)

val serialize			: t -> bytes
(** Serialize a block *)



