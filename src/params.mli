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
val name_of_network 	: e -> string
val abbr_to_network		: string -> e
