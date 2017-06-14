open Stdint;;
open Bitstring;;
open Crypto;;
open Parser;;


module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		time		: float;
		bits		: uint32;
		nonce		: uint32;	
	};;

	let serialize h = 
		let btime = Bytes.create 4 in
		Uint32.to_bytes_little_endian (Uint32.of_float h.time) btime 0;
		let bbits = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.bits bbits 0;
		let bnonce = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.nonce bnonce 0;
		let bs = BITSTRING {
			h.version 							: 4*8 : littleendian;
			Hash.to_bin h.prev_block			: 32*8: string; 
			Hash.to_bin h.merkle_root			: 32*8: string;
			btime								: 32 : string;
			bbits								: 32 : string;
			bnonce								: 32 : string
		} in Bitstring.string_of_bitstring bs
	;;
	
	let parse data = 
		let check_target h b =
			let calc_target b = 
				let exp = Int64.of_bytes_little_endian ((Bytes.sub b 0 1) ^ (Bytes.make 7 (Char.chr 0))) 0 in
				let body = Int64.of_bytes_little_endian ((Bytes.sub b 1 3) ^ (Bytes.make 5 (Char.chr 0))) 0 in
				Big_int.power_big_int_positive_big_int (Big_int.big_int_of_int64 body) (Big_int.big_int_of_int64 exp)
			in
			let t' = calc_target b in
			let h' = Hash.to_bigint h in
			Big_int.lt_big_int h' t'
		in
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			version 	: 4*8 : littleendian;
			prev_block	: 32*8: string; 
			merkle_root	: 32*8: string;
			time		: 32 : string;
			bits		: 32 : string;
			nonce		: 32 : string
		} -> 
			let hash = Hash.of_bin (hash256 data) in
			if check_target hash bits then Some ({
				hash			= hash;
				version			= version;
				prev_block		= Hash.of_bin prev_block;
				merkle_root		= Hash.of_bin merkle_root;
				time			= Uint32.to_float (Uint32.of_bytes_little_endian time 0);
				bits			= Uint32.of_bytes_little_endian bits 0;
				nonce			= Uint32.of_bytes_little_endian nonce 0;
			})
			else None
		| { _ } -> None
	;;
end

type t = {
	header	: Header.t;
	txs		: Tx.t list;
};;


let parse data =
	let header = Header.parse (Bytes.sub data 0 80) in
	match header with
	| None -> None
	| Some (header) ->
		let bdata = bitstring_of_string  (Bytes.sub data 80 ((Bytes.length data) - 80)) in
		let txn, rest' = parse_varint bdata in
		let txs = Tx.parse_all (string_of_bitstring rest') (Uint64.to_int txn) in
		match txs with 
		| Some (txs) -> Some ({ header= header; txs= txs; })
		| None -> None
;;



let serialize block = 
	let d = Header.serialize (block.header) in
	let d = Bytes.cat d (string_of_bitstring (bitstring_of_varint (Int64.of_int (List.length block.txs)))) in
	Bytes.cat d (Tx.serialize_all block.txs)	
;;