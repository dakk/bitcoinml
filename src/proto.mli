open Stdint

type header  = {
	magic		: int32;
	command		: string;
	length		: uint32;
	checksum	: string;
}

type invvect = 
	INV_ERROR
	| INV_TX of Hash.t
	| INV_BLOCK of Hash.t
	| INV_FILTERED_BLOCK of Hash.t

type addr = {
	services	: uint64;
	address		: string;
	port		: uint16;
}

type inv = invvect list

type getdata = inv
type notfound = inv

type version = {
	version		: int32;
	services	: int64;
	time		: float;
	addr_recv	: addr;
	addr_from	: addr;
	nonce		: int64;
	user_agent	: string;
	start_height: int32;
	relay		: bool;
}



type getheaders = {
	version		: int32;
	hashes		: Hash.t list;
	stop		: Hash.t;
}

type getblocks = getheaders


type headers = Block.Header.t list

type ping = int64
type pong = int64


type t = 
	| INVALID
	| VERSION of version
	| VERACK
	| PING of ping
	| PONG of pong
	| INV of inv
	| ADDR
	| GETDATA of inv
	| NOTFOUND of notfound
	| GETBLOCKS of getblocks
	| GETHEADERS of getheaders
	| TX of Tx.t
	| BLOCK of Block_lazy.t
	| HEADERS of headers
	| GETADDR
	| MEMPOOL
	| REJECT
	| FEEFILTER of Uint64.t
	| SENDHEADERS
	
	(* Bloom filter related *)
	| FILTERLOAD
	| FILTERADD
	| FILTERCLEAR
	| MERKLEBLOCK
	
	| ALERT
;;
(** Protocol message type *)


val string_of_command : t -> string
(** [string_of_command m] returns the command name of the message *)

val parse		: header -> string -> t
(** [parse h data] parses [data] and returns the message *)

val parse_header: string -> header
(** [parse_header h] parses the header [h] *)

val bitstring_of_addr 	: addr -> Bitstring.t
(** [bitstring_of_addr addr] returns the bitstring of an address [addr] *)


val serialize_header : header -> string
(** [serialize_header h] serializes the message header [h] *)

val serialize_message   : t -> string
(** [serialize_message m] serializes the message [m] *)

val serialize			: Params.t -> t -> string
(** [serialize p m] serializes the message [m] encapsulating it with the header comptabile with params [p] *)