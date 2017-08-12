open Stdint
open Bitstring
open Sexplib
open Conv
open Convhelper


module In : sig
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;
	} [@@deriving sexp] 

	val parse 			: ?coinbase:bool -> bitstring -> bitstring * t option
	val parse_all		: ?coinbase:bool -> bitstring -> bitstring * t list option
	val serialize		: t -> bytes
	val serialize_all	: t list -> bytes
	val to_string		: t -> string
end

module Out : sig
	type t = {
		value	: int64;
		script	: Script.t;
	} [@@deriving sexp]

	val parse			: bitstring -> bitstring * t option
	val parse_all		: bitstring -> bitstring * t list option
	val serialize		: t -> bytes
	val serialize_all	: t list -> bytes

	val is_spendable	: t -> bool
	val spendable_by	: t -> Address.prefix -> string option
	val to_string			: t -> string
end


type t = {
	hash			: Hash.t;
	version		: int32;
	txin 			: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
	size			: int;
} [@@deriving sexp]

val parse 			: ?coinbase:bool -> bytes -> bytes * t option
val parse_all		: bytes -> int -> t list option

val serialize		: t -> bytes
val serialize_all	: t list -> bytes

val to_string		: t -> string

