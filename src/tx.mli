open Stdint
open Bitstring


module In : sig
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;
	}

	val parse 			: ?coinbase:bool -> bitstring -> bitstring * t option
	val parse_all		: ?coinbase:bool -> bitstring -> bitstring * t list option
	val serialize		: t -> bytes
	val serialize_all	: t list -> bytes
end

module Out : sig
	type t = {
		value	: int64;
		script	: Script.t;
	}

	val parse			: bitstring -> bitstring * t option
	val parse_all		: bitstring -> bitstring * t list option
	val serialize		: t -> bytes
	val serialize_all	: t list -> bytes

	val is_spendable	: t -> bool
	val spendable_by	: t -> string option
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
}

val parse 			: ?coinbase:bool -> bytes -> bytes * t option
val parse_all		: bytes -> int -> t list option

val serialize		: t -> bytes
val serialize_all	: t list -> bytes

val print			: t -> unit