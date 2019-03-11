open Stdint;;
open Bitstring;;
open Bytes;;
open Varint;;
open Hash;;

module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Merkle.t;
		time		: float;
		bits		: string;
		nonce		: uint32;
	};;

	let serialize h =
		let btime = Bytes.create 4 in
		Uint32.to_bytes_little_endian (Uint32.of_float h.time) btime 0;
		let bnonce = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.nonce bnonce 0;
		let%bitstring bs = {|
			h.version 							: 4*8 : littleendian;
			Hash.to_bin h.prev_block			: 32*8: string;
			Hash.to_bin h.merkle_root			: 32*8: string;
			to_string btime				: 32 : string;
			Hash.to_bin_norev h.bits			: 32 : string;
			to_string bnonce				: 32 : string
		|} in Bitstring.string_of_bitstring bs
	;;

	let check_target h =
		let calc_target b =
			let b = Hash.to_bin_norev b in 
			let exp = (String.get b 3 |> Char.code) - 3 in
			let mantissa = (String.make 1 @@ String.get b 2) ^ (String.make 1 @@ String.get b 1) ^ (String.make 1 @@ String.get b 0) in
			let n = if 32 - exp - 3 > 0 then
					String.make (32 - exp - 3) (Char.chr 0) ^ mantissa ^ String.make exp (Char.chr 0) 
				else
					mantissa ^ String.make exp (Char.chr 0)
			in
			Hash.of_bin_norev n
		in
		let rec check h t = match String.length h with
		| 0 -> true
		| n ->
			let fh = String.get h 0 in
			let ft = String.get t 0 in
			let resth = if n = 1 then "" else String.sub h 1 (n-1) in
			let restt = if n = 1 then "" else String.sub t 1 (n-1) in
			match (fh, ft) with
			| '0', '0' -> check resth restt 
			| '0', b when b <> '0' -> true
			| a, '0' when a <> '0' -> false
			| a, b when a = b -> check resth restt
			| a, b -> 
				let a' = Scanf.sscanf (String.make 1 a) "%1x" (fun i -> i) in
				let b' = Scanf.sscanf (String.make 1 b) "%1x" (fun i -> i) in
				if a' > b' then false else true
		in check h.hash @@ calc_target h.bits
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
				time			= Uint32.to_float (Uint32.of_bytes_little_endian (of_string time) 0);
				bits			= Hash.of_bin_norev bits;
				nonce			= Uint32.of_bytes_little_endian (of_string nonce) 0;
			})
		| {| _ |} -> None
	;;
end

type t = {
	header	: Header.t;
	txs			: Tx.t list;
	size		: int;
};;




let parse ?(hex=false) bdata =
	let data = (if hex then Hex.to_string @@ `Hex bdata else bdata) in
	let header = Header.parse (String.sub data 0 80) in
	match header with
	| None -> None
	| Some (header) ->
		let bdata = bitstring_of_string  (String.sub data 80 ((String.length data) - 80)) in
		let txn, rest' = parse_varint bdata in
		let txs = Tx.parse_all (string_of_bitstring rest') (Uint64.to_int txn) in
		match txs with
		| Some (txs) -> Some ({ header= header; txs= List.rev txs; size= String.length data })
		| None -> None
;;

let parse_legacy ?(hex=false) bdata =
	let data = (if hex then Hex.to_string @@ `Hex bdata else bdata) in
	let header = Header.parse (String.sub data 0 80) in
	match header with
	| None -> None
	| Some (header) ->	
		let bdata = bitstring_of_string  (String.sub data 80 ((String.length data) - 80)) in
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



let serialize ?(hex=false) block =
	let d = Header.serialize (block.header) in
	let d = String.concat "" [d; string_of_bitstring (bitstring_of_varint (Int64.of_int (List.length block.txs)))] in
	let bdata = String.concat "" [d; Tx.serialize_all block.txs] in
	if hex then Hex.of_string bdata |> Hex.show else bdata
;;