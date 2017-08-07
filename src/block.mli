open Stdint

module Merkle : sig
	type t = Hash.t [@@deriving sexp]

	val of_txs    : Tx.t list -> t
	val of_hashes : Hash.t list -> t
end

module Header : sig
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		time		: float;
		bits		: uint32;
		nonce		: uint32;
	} [@@deriving sexp]

	val parse 		: bytes -> t option
	val serialize	: t -> bytes
	val to_string	: t -> string
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		:	int;
} [@@deriving sexp]

val parse		: bytes -> t option
val serialize	: t -> bytes
val to_string	: t -> string
