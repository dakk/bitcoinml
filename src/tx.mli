(** Bitcoin transaction *)

open Stdint
open Bitstring

val amount_check			: ?max_money:Int64.t -> Int64.t -> bool
(** [amount_check ~max_money:Int64.t num] checks if [num] exceeds [~max_money] or 21M *)

(** Transaction input *)
module In : sig
	type t = {
		out_hash			: string;
		out_n					: uint32;
		script				: Script.t;
		witness_script: Script.data list option;
		sequence			: uint32;
	}

	val parse 			: ?coinbase:bool -> bitstring -> bitstring * t option
	(** [parse ~coinbase:bool bdata] parses an input from [bdata] *)

	val parse_all		: ?coinbase:bool -> bitstring -> bitstring * t list option
	(** [parse_all ~coinbase:bool bdata] parses a list of inputs from [bdata] *)
	
	val serialize		: t -> string
	(** [serialize inp] serializes the input [inp] *)

	val serialize_all	: t list -> string
	(** [serialize_all inpl] serializes a list of inputs [inpl] *)

	val has_witness	: t -> bool
	(** [has_witness inp] returns true if the input has witness data *)
end

(** Transaction output *)
module Out : sig
	type t = {
		value	: int64;
		script	: Script.t;
	}

	val parse			: bitstring -> bitstring * t option
	(** [parse bdata] parses an output from [bdata] *)

	val parse_all		: bitstring -> bitstring * t list option
	(** [parse_all bdata] parses a list of outputs from [bdata] *)

	val serialize		: t -> string
	(** [serialize out] serializes the output [out] *)

	val serialize_all	: t list -> string
	(** [serialize_all outs] serializes a list of outputs [outs] *)

	val is_spendable	: t -> bool
	(** [is_spendable out] returns true if the [out] is spendable by a known template *)

	val spendable_by	: t -> Address.prefix -> string option
	(** [spendable_by out prefix] returns the address (with [prefix]) which is able to spend [out] *)
end

(** Witness data *)
module Witness : sig 
	type t = {
		hash		: Hash.t;
		marker	: int;
		flag		: int;
		size		: int;
	}

	val serialize_fields 	: In.t list -> string
	(** [serialize_fields inl] serializes witness fields of [inl] inputs *)
	
	val parse_fields			: bitstring -> int -> bitstring * string list list option
	(** [parse_fields bdata] parses [n] witness fields in [bdata] and returns the rest 
			of the data and the list of fields *)
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
}

val parse 						: ?coinbase:bool -> ?hex:bool -> string -> string * t option
(** [parse ~coinbase:bool ~hex:bool data] parses a binary [data] and returns the parsed 
		transaction; [data] could be a binary data, or an hex human readable string if [~hex] is true *)

val parse_legacy			: ?coinbase:bool -> ?hex:bool -> string -> string * t option
(** [parse_legacy ~coinbase:bool ~hex:bool data]; same as [parse] but disabling segwit *)

val parse_all					: string -> int -> t list option
(** [parse_all data n] parses a list of [n] transactions encoded in [data] *)

val parse_all_legacy	: string -> int -> t list option
(** [parse_all_legacy data n]; same as [parse_all] but disabling segwit *)


val serialize							: ?hex:bool -> t -> string
(** [serialize ~hex:bool tx] serializes the transaction [tx]; the result is a binary 
		data, or an hex human readable string if [~hex] is true *)

val serialize_legacy			: ?hex:bool -> t -> string
(** [serialize_legacy ~hex:bool tx]; same as [serialize] but disabling segwit *)

val serialize_all					: t list -> string
(** [serialize_all txl] serializes a list of transaction [txl] *)

val serialize_all_legacy	: t list -> string
(** [serialize_all_legacy txl]; same as [serialize_all] but disabling segwit *)

val is_witness	: t -> bool
(** [is_witness tx] returns true if [tx] hash witness data *)

val is_coinbase	: t -> bool
(** [is_coinbase tx] returns true if [tx] is a coinbase transaction *)

