open Stdint;;
open Bitstring;;
open Varint;;
open Hash;;
open Sexplib;;
open Conv;;
open Conv_helper;;

module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Merkle.t;
		time		: float;
		bits		: uint32;
		nonce		: uint32;
	} [@@deriving sexp];;

	let to_string h = sexp_of_t h |> Sexp.to_string;;

	let serialize h =
		let btime = Bytes.create 4 in
		Uint32.to_bytes_little_endian (Uint32.of_float h.time) btime 0;
		let bbits = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.bits bbits 0;
		let bnonce = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.nonce bnonce 0;
		let%bitstring bs = {|
			h.version 							: 4*8 : littleendian;
			Hash.to_bin h.prev_block			: 32*8: string;
			Hash.to_bin h.merkle_root			: 32*8: string;
			btime								: 32 : string;
			bbits								: 32 : string;
			bnonce								: 32 : string
		|} in Bitstring.string_of_bitstring bs
	;;

	let check_target h =
		let calc_target b =
			let exp = Int64.of_bytes_little_endian ((Bytes.sub b 0 1) ^ (Bytes.make 7 (Char.chr 0))) 0 in
			let body = Int64.of_bytes_little_endian ((Bytes.sub b 1 3) ^ (Bytes.make 5 (Char.chr 0))) 0 in
			Big_int.power_big_int_positive_big_int (Big_int.big_int_of_int64 body) (Big_int.big_int_of_int64 exp)
		in
		let buf = Bytes.create 32 in
		let _ = Uint32.to_bytes_little_endian h.bits buf 0 in
		let t' = calc_target buf in
		let h' = Hash.to_bigint h.hash in
		Big_int.lt_big_int h' t'
	;;

	let parse data =
		let bdata = bitstring_of_string data in
		match%bitstring bdata with
		| {|
			version 	: 4*8 : littleendian;
			prev_block	: 32*8: string;
			merkle_root	: 32*8: string;
			time		: 32 : string;
			bits		: 32 : string;
			nonce		: 32 : string
		|} ->
			let hash = Hash.of_bin (hash256 data) in
			Some ({
				hash			= hash;
				version			= version;
				prev_block		= Hash.of_bin prev_block;
				merkle_root		= Hash.of_bin merkle_root;
				time			= Uint32.to_float (Uint32.of_bytes_little_endian time 0);
				bits			= Uint32.of_bytes_little_endian bits 0;
				nonce			= Uint32.of_bytes_little_endian nonce 0;
			})
		| {| _ |} -> None
	;;
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		: int;
} [@@deriving sexp];;




let parse data =
	let header = Header.parse (Bytes.sub data 0 80) in
	match header with
	| None -> None
	| Some (header) ->
		let bdata = bitstring_of_string  (Bytes.sub data 80 ((Bytes.length data) - 80)) in
		let txn, rest' = parse_varint bdata in
		let txs = Tx.parse_all (string_of_bitstring rest') (Uint64.to_int txn) in
		match txs with
		| Some (txs) -> Some ({ header= header; txs= List.rev txs; size= Bytes.length data })
		| None -> None
;;

let parse_legacy data =
	let header = Header.parse (Bytes.sub data 0 80) in
	match header with
	| None -> None
	| Some (header) ->	
		let bdata = bitstring_of_string  (Bytes.sub data 80 ((Bytes.length data) - 80)) in
		let txn, rest' = parse_varint bdata in
		let txs = Tx.parse_all_legacy (string_of_bitstring rest') (Uint64.to_int txn) in
		match txs with
		| Some (txs) -> Some ({ 
			header= header; 
			txs= List.rev txs; 
			size= 80 + Varint.encoding_length txn + List.fold_left (fun a x -> a + x.Tx.size) 0 txs;
		})
		| None -> None
;;



let serialize block =
	let d = Header.serialize (block.header) in
	let d = Bytes.cat d (string_of_bitstring (bitstring_of_varint (Int64.of_int (List.length block.txs)))) in
	Bytes.cat d (Tx.serialize_all block.txs)
;;


let to_string b = sexp_of_t b |> Sexp.to_string;;

