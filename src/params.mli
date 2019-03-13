(** Params handles different protocol parameters for different Bitcoin based blockchains *)

open Stdint
type e = BTC | XTN | BCH | LTC | LTN | SIDECHAIN | NOTFOUND

type t = { 
	hash_function	: string -> string;
	block_size	: int;
	block_time	: int;
	genesis			: Block.Header.t;
	magic				: int;
	port				: int;
	seeds				: string list;
	network			: e;
	checkpoints	: (int * Hash.t) list;
	prefixes		: Address.prefix;
	max_money		: Int64.t;
}

val of_network 			: e -> t
(** [of_network e] returns the params of the [e] network *)

val name_of_network 	: e -> string
(** [name_of_network e] returns the readable name of the [e] network *)

val abbr_to_network		: string -> e
(** [abbr_to_network ab] returns the network of a readable abbreviation *)
