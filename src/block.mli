(** Block serialization and parsing module *)

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

	val parse 		: ?hex:bool -> string -> t option
	(** [parse ~hex:bool data] parses the [data] and returns a [t] option; [data] could be
			a binary data, or an hex human readable string if [~hex] is true *)

	val serialize	: ?hex:bool -> t -> string
	(** [serialize ~hex:bool header] serializes a block [header] and returns a binary string
			or an hex human readable string if [~hex] is true *)

	val check_target : t -> bool
	(** [check_target header] checks the nbits / hash target *)
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		:	int;
}


val parse					: ?hex:bool -> string -> t option
(** [parse ~hex:bool data] parses the [data] and returns a [t] option; [data] could be
    a binary data, or an hex human readable string if [~hex] is true *)

val parse_legacy	: ?hex:bool -> string -> t option
(** [parse_legacy ~hex:bool data]; same as [parse] but disable segwit *)	

val serialize			: ?hex:bool -> t -> string
(** [serialize ~hex:bool block] serializes a block [block] and returns a binary string
		or an hex human readable string if [~hex] is true *)



