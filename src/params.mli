open Stdint
type e = BTC | XTN | BCH | SIDECHAIN | NOTFOUND

type t = { 
	block_size	: int;
	genesis			: Block.Header.t;
	magic				: int;
	port				: int;
	seeds				: string list;
	network			: e;
	checkpoints	: (int * Hash.t) list;
	prefixes		: Address.prefix;
}

val of_network 			: e -> t
val name_of_network 	: e -> string
val abbr_to_network		: string -> e
