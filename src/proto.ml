open Bitstring;;
open Stdint;;
open Params;;
open Hash;;


type header = {
	magic		: int32;
	command		: string;
	length		: uint32;
	checksum	: string;
};;


type invvect = 
		INV_ERROR
	| INV_TX of Hash.t
	| INV_BLOCK of Hash.t
	| INV_FILTERED_BLOCK of Hash.t

type addr = {
	services	: uint64;
	address		: string;
	port		: uint16;
};;


type inv = invvect list;;

type getdata = inv;;
type notfound = inv;;

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
};;

type getheaders = {
	version		: int32;
	hashes		: Hash.t list;
	stop		: Hash.t;
}

type getblocks = getheaders

type headers = Block.Header.t list

type ping = int64;;
type pong = int64;;


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


let string_of_command c = match c with
	| INVALID -> "invalid"
	| VERSION (v) -> "version"
	| VERACK -> "verack"
	| PING (p) -> "ping"
	| PONG (p) -> "pong"
	| INV (i) -> "inv"
	| ADDR -> "addr"
	| GETDATA (gd) -> "getdata"
	| NOTFOUND (nf) -> "notfound"
	| GETBLOCKS (gb) -> "getblocks"
	| GETHEADERS (gh) -> "getheaders"
	| TX (tx) -> "tx"
	| BLOCK (b) -> "blocks"
	| HEADERS (h) -> "headers"
	| GETADDR -> "getaddr"
	| MEMPOOL -> "mempool"
	| REJECT -> "reject"
	| FEEFILTER (f) -> "feefilter"
	| SENDHEADERS -> "sendheaders"
	
	(* Bloom filter related *)
	| FILTERLOAD -> "filterload"
	| FILTERADD -> "filteradd"
	| FILTERCLEAR -> "filterclear"
	| MERKLEBLOCK -> "merkleblock"
	
	| ALERT -> "alert"
;;




(******************************************************************)
(* Parsing ********************************************************)
(******************************************************************)
let string_from_zeroterminated_string zts =
	let string_length =
 		try String.index zts '\x00' with Not_found -> 12
	in String.sub zts 0 string_length
;;


let parse_varstring bits =
	let length, bits = Varint.parse_varint bits in
	if length = Uint64.zero then ("", bits)
	else
		match%bitstring bits with
		| {| value : (Uint64.to_int length) * 8 : string;
				rest : -1 : bitstring
			|} -> (value, rest)
		| {| _ |} -> ("", bits)
;;



let parse_headers data = 
	let rec ph' data n acc =
		if n = Uint64.zero then acc else
			match%bitstring data with
			| {| raw : 80 * 8 : string; txc : 1 * 8: littleendian; rest : -1 : bitstring |} ->
				let blockh = Block.Header.parse raw in
				match blockh with
				| None -> ph' rest (Uint64.sub n Uint64.one) acc
				| Some (header) -> ph' rest (Uint64.sub n Uint64.one) (header::acc)
	in  
	let bdata = bitstring_of_string data in
	let count, rest = Varint.parse_varint bdata in
	(ph' rest count [])
;;


let parse_inv data =
	let rec parse_invvects bdata count acc = 
		if count = 0 then acc else (
		match%bitstring bdata with 
		| {| 
			itype		: 4*8 : littleendian;
			hash		: 32*8: string;
			rest		: -1  : bitstring
		|} ->
			let iv = match (Int32.to_int itype) with
					1 -> INV_TX  (Hash.of_bin hash)
				| 2 -> INV_BLOCK (Hash.of_bin hash)
				| 3 -> INV_FILTERED_BLOCK (Hash.of_bin hash)
				| _ -> INV_ERROR
			in parse_invvects rest (count - 1) (iv::acc)
		)
	in
	let bdata = bitstring_of_string data in
	let count, rest = Varint.parse_varint bdata in
	parse_invvects rest (Uint64.to_int count) []
;;

let parse_notfound data = parse_inv data;;
let parse_getdata data = parse_inv data;;

let parse_netaddr data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {|
		time		: 4*8 : string;
		services	: 8*8 : littleendian;
		address		: 16*8 : string;
		port		: 2*8 : littleendian
	|} -> 
		{ 
			address="0000000000000000" ; 
			services= Uint64.of_int64 services; 
			port= Uint16.of_int port 
		}
;;

let parse_version data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with 
	| {|
		version 			: 4*8 : littleendian;
		services 			: 8*8 : littleendian;
		time 					: 8*8 : littleendian;
		addr_recv			: 26*8 : string;
		addr_from	 		: 26*8 : string;
		nonce					: 8*8 : littleendian;
		rest					: -1 : bitstring
	|} -> 
		match parse_varstring rest with
		| (user_agent, rest) ->
			(match%bitstring rest with 
			| {|
				start_height		: 4*8 : littleendian;
				relay				: 1*8 : littleendian						
			|} -> {
				addr_recv= { 
					address="0000000000000000" ; 
					services= Uint64.of_int 0; 
					port= Uint16.of_int 0
				} (*parse_netaddr addr_recv;*);
				addr_from= { 
					address="0000000000000000" ; 
					services= Uint64.of_int 0; 
					port= Uint16.of_int 0
				} (*parse_netaddr addr_from;*);
				version= version;
				services= services;
				time= Int64.to_float (time);
				nonce= nonce;
				user_agent= user_agent;
				start_height= start_height;
				relay= false
			}
		)
;;

let parse_ping data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {| nonce		: 8*8	: littleendian |} -> nonce
	| {| _ |} -> raise (Invalid_argument "Invalid ping message")
;;

let parse_pong data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {| nonce		: 8*8	: littleendian |} -> nonce
	| {| _ |} -> raise (Invalid_argument "Invalid pong message")
;;

let parse_feefilter data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {| feerate		: 8*8	: littleendian |} -> Uint64.of_int64 feerate
	| {| _ |} -> raise (Invalid_argument "Invalid feefilter message")
;;



let parse_getheaders data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with 
	| {|
		version : 4*8 : littleendian
	|} ->
		(*rest : -1 : bitstring*)
		let _ = Varint.parse_varint bdata in
		{
			version= version;
			hashes= [];
			stop= "";
		}
;;

let parse_getblocks data = parse_getheaders data;;

let parse_header data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {| 
		magic 		: 4*8 	: littleendian;
		command 	: 12*8 	: string;
		length 		: 4*8 	: littleendian;
		checksum	: 4*8 	: string
	|} ->
	{
		magic 		= magic;
		command 	= string_from_zeroterminated_string command;
		length 		= Uint32.of_int32 length;
		checksum	= checksum;
	}
	| {| _ |} -> raise (Invalid_argument "Invalid protocol header")
;;


let parse header payload = 
	match header.command with
	| "version" -> VERSION (parse_version payload)
	| "ping" -> PING (parse_ping payload)
	| "pong" -> PONG (parse_pong payload)
	| "headers" -> HEADERS (parse_headers payload)
	| "verack" -> VERACK
	| "getaddr" -> GETADDR
	| "mempool" -> MEMPOOL
	| "sendheaders" -> SENDHEADERS
	| "getheaders" -> GETHEADERS (parse_getheaders payload)
	| "getblocks" -> GETBLOCKS (parse_getblocks payload)
	| "inv" -> INV (parse_inv payload)
	| "feefilter" -> FEEFILTER (parse_feefilter payload)
	| "tx" -> (
		match Tx.parse payload with
		| rest, None -> INVALID
		| rest, Some (tx) -> TX (tx)
	)
	| "block" -> (
		match Block_lazy.parse payload with
		| Some (bl) -> BLOCK (bl)
		| None -> INVALID
	)
	| "getdata" -> GETDATA (parse_getdata payload)
	| "addr" -> ADDR
	| "notfound" -> NOTFOUND (parse_notfound payload)
	| _ -> raise (Invalid_argument ("Protocol command " ^ header.command ^ " not recognized"))
;;







(******************************************************************)
(* Serialization **************************************************)
(******************************************************************)
let bitstring_of_addr (addr: addr) : Bitstring.t =
	[%bitstring {|
		Uint64.to_int64 addr.services	: 8*8 	: littleendian;
		addr.address	: 16*8 	: string;
		Uint16.to_int addr.port		: 2*8 	: littleendian
	|}]
;;


let bitstring_of_varstring s = 
	match String.length s with
	| 0 -> bitstring_of_string "\x00"
	| n -> 
		let length_varint_bitstring = Varint.bitstring_of_varint (Int64.of_int (String.length s)) in
		[%bitstring {|
			length_varint_bitstring : -1 : bitstring;
			s 						: (String.length s) * 8 : string
		|}]
;;

let int_of_bool b = 
	match b with
	| true -> 1
	| false -> 0
;;

let serialize_getheaders v = 
	[%bitstring {|
		v.version 												: 4*8 : littleendian;
		Varint.bitstring_of_varint (Int64.of_int (List.length v.hashes))	: -1 : bitstring;
		Hash.to_bin (List.nth v.hashes 0)						: 32*8 : string;
		Hash.to_bin (v.stop)									: 32*8 : string
	|}] 
;;

let serialize_getblocks v = serialize_getheaders v;;

let serialize_version (v:version) =
	[%bitstring {|
		v.version 										: 4*8 : littleendian;
		v.services 										: 8*8 : littleendian;
		Int64.of_float (v.time) 						: 8*8 : littleendian;
		(bitstring_of_addr v.addr_recv)					: -1 : bitstring;
		(bitstring_of_addr v.addr_from)			 		: -1 : bitstring;
		v.nonce											: 8*8 : littleendian;
		bitstring_of_varstring v.user_agent 			: -1 : bitstring;
		v.start_height 									: 4*8 : littleendian;
		int_of_bool true								: 1*8 : littleendian
	|}]
;;

let serialize_ping p = [%bitstring {| p 	: 8*8 : littleendian |}];;
let serialize_pong p = [%bitstring {| p 	: 8*8 : littleendian |}];;


let serialize_getdata gd = 
	let ser_iv v = 
		let it = match v with
		| INV_BLOCK (h) -> (h, 2)
		| INV_TX (h) -> (h, 1)
		| INV_FILTERED_BLOCK (h) -> (h, 3)
		| INV_ERROR -> ("", 0)		
		in [%bitstring {|
			Int32.of_int (snd (it)) : 4*8 : littleendian;
			Hash.to_bin (fst (it)) : 32*8 : string
		|}]
	in
	let rec ser_ivs vl =
		match vl with
		| [] -> []
		| v::vl' -> (ser_iv v)::(ser_ivs vl')
	in
	let invvects = ser_ivs gd in
	[%bitstring {|
		Varint.bitstring_of_varint (Int64.of_int (List.length gd)): -1 : bitstring;
		Bitstring.concat invvects 						: -1 : bitstring
	|}]
;;

let serialize_inv gd = serialize_getdata gd;;
let serialize_notfound nf = serialize_getdata nf;;

let serialize_header header =
	let blength = Bytes.create 4 in
	let _ = Uint32.to_bytes_little_endian header.length blength 0 in
	let bdata = [%bitstring {|
		header.magic 	: 4*8 	: littleendian;
		header.command	: 12*8 	: string;
		Bytes.to_string blength	: 4*8 	: string;
		header.checksum : 4*8 	: string
	|}] in string_of_bitstring bdata
;;


let serialize_message message = 
	let bdata = match message with
	| PING (p) -> serialize_ping p
	| PONG (p) -> serialize_pong p
	| VERSION (v) -> serialize_version v
	| VERACK -> empty_bitstring
	| GETHEADERS (gh) -> serialize_getheaders gh
	| GETBLOCKS (gb) -> serialize_getblocks gb
	| GETDATA (gd) -> serialize_getdata gd
	| INV (gd) -> serialize_inv gd
	| NOTFOUND (nf) -> serialize_notfound nf	
	| GETADDR -> empty_bitstring
	| MEMPOOL -> empty_bitstring
	| SENDHEADERS -> empty_bitstring
	| TX (tx) -> bitstring_of_string @@ Tx.serialize tx
	| _ -> empty_bitstring
	in string_of_bitstring bdata
;;


let serialize params message = 
	let mdata = serialize_message message in
	let command = string_of_command message in
	let command' = command ^ (String.make (12 - (String.length command)) '\x00') in
	let header = {
		magic	= Int32.of_int params.Params.magic;
		command	= command';
		length	= Uint32.of_int (String.length mdata);
		checksum= Hash.checksum4 mdata;
	} in 
	let hdata = serialize_header header in
	String.concat "" [hdata; mdata]
;;