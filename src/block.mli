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
	} [@@deriving sexp]

	val parse 		: bytes -> t option
	val serialize	: t -> bytes
	val to_string	: t -> string
	val check_target : t -> bool
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		:	int;
} [@@deriving sexp]


val parse					: bytes -> t option
(** Parse a block *)

val parse_legacy	: bytes -> t option
(** Parse a legacy block *)

val serialize			: t -> bytes
(** Serialize a block *)

val to_string			: t -> string
(** Get a string sexp representation of a block *)


module LazyBlock : sig
	type l = {
		header	: Header.t;
		ltxs		: Tx.t list option Lazy.t;
		size		: int
	}

	val parse				: bytes -> l option
	(** Lazy parse of a block *)
	
	val parse_legacy	: bytes -> l option
	(** Lazy parse of a legacy block *)	

	val force					: l -> t option
	(** Force a lazy eval *)

	val force_option	: l option -> t option
	(** Force a lazy eval of an option lazy block *)
end