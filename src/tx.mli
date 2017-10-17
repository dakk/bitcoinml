open Stdint
open Bitstring
open Sexplib
open Conv
open Conv_helper

val amount_check			: ?max_money:Int64.t -> Int64.t -> bool

module In : sig
	type t = {
		out_hash			: string;
		out_n					: uint32;
		script				: Script.t;
		witness_script: Script.data list option;
		sequence			: uint32;
	} [@@deriving sexp] 

	val parse 			: ?coinbase:bool -> bitstring -> bitstring * t option
	val parse_all		: ?coinbase:bool -> bitstring -> bitstring * t list option
	val serialize		: t -> bytes
	val serialize_all	: t list -> bytes
	val to_string		: t -> string
	val has_witness	: t -> bool
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

module Witness : sig 
	type t = {
		hash		: Hash.t;
		marker	: int;
		flag		: int;
		size		: int;
	} [@@deriving sexp]

	val serialize_fields 	: In.t list -> bytes
	val parse_fields			: bitstring -> int -> bitstring * bytes list list option
end


type t = {
	hash			: Hash.t;
	version		: int32;
	txin 			: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
	size			: int;
	vsize			: int;
	witness		: Witness.t option;
} [@@deriving sexp]

val parse 						: ?coinbase:bool -> bytes -> bytes * t option
val parse_legacy			: ?coinbase:bool -> bytes -> bytes * t option
val parse_all					: bytes -> int -> t list option
val parse_all_legacy	: bytes -> int -> t list option

val serialize							: t -> bytes
val serialize_legacy			: t -> bytes
val serialize_all					: t list -> bytes
val serialize_all_legacy	: t list -> bytes

val is_witness	: t -> bool
val is_coinbase	: t -> bool
val to_string		: t -> string


