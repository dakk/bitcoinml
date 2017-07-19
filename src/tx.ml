open Stdint;;
open Bitstring;;
open Varint;;


module In = struct
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;
	};;

	let serialize txin =
		let out = Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin txin.out_hash : 32 * 8 : string;
			Uint32.to_int32 txin.out_n : 32 : littleendian
		|}]) in
		let sclen = string_of_bitstring (Varint.bitstring_of_varint (Int64.of_int (Script.length txin.script))) in
		let sequence = Bitstring.string_of_bitstring ([%bitstring {| Uint32.to_int32 txin.sequence : 32 : littleendian |}]) in
		out ^ sclen ^ Script.serialize (txin.script) ^ sequence
	;;

	let serialize_all txins =
		let rec serialize_all' ins = match ins with
		| [] -> ""
		| i::ins' -> (serialize i) ^ (serialize_all' ins')
		in
		let len = string_of_bitstring (Varint.bitstring_of_varint (Int64.of_int (List.length txins))) in
		len ^ (serialize_all' txins)
	;;

	let parse ?(coinbase=false) bdata =
		match%bitstring bdata with
		| {|
			out_hash	: 32*8: string;
			out_n		: 32 : littleendian;
			rest		: -1 : bitstring
		|} ->
			let sclen, rest' = parse_varint rest in
			match%bitstring rest' with
			| {|
				script 		: Uint64.to_int (sclen) * 8 : string;
				sequence	: 32 : littleendian;
				rest'		: -1 : bitstring
			|} ->
				let sc = match coinbase with
					| true -> Script.parse_coinbase script
					| false -> Script.parse script
				in

				(rest', Some ({
					out_hash= Hash.of_bin out_hash;
					out_n= Uint32.of_int32 out_n;
					script= sc;
					sequence= Uint32.of_int32 sequence;
					}))

		| {| _ |} -> (bitstring_of_string "", None)
	;;

	let parse_all ?(coinbase=false) data =
		let inlen, rest' = parse_varint data in
		let rec parse_all' n d acc = match n with
		| 0 -> (d, Some (acc))
		| n ->
			let rest, txin = parse ~coinbase:coinbase d in
			match txin with
			| None -> (bitstring_of_string "", None)
			| Some (txin) -> parse_all' (n-1) rest (txin::acc)
		in parse_all' (Uint64.to_int inlen) rest' []
	;;
end

module Out = struct
	type t = {
		value	: int64;
		script	: Script.t;
	};;

	let is_spendable txout = Script.is_spendable txout.script;;

	let spendable_by txout = Script.spendable_by txout.script;;

	let serialize txout =
		let value = Bitstring.string_of_bitstring ([%bitstring {| txout.value : 64 : littleendian |}]) in
		let sclen = string_of_bitstring @@ Varint.bitstring_of_varint (Int64.of_int (Script.length txout.script)) in
		let sc = Script.serialize (txout.script) in
		value ^ sclen ^ sc
	;;

	let serialize_all txouts =
		let rec serialize_all' outs = match outs with
		| [] -> ""
		| o::outs' -> (serialize o) ^ (serialize_all' outs')
		in
		let len = string_of_bitstring @@ Varint.bitstring_of_varint (Int64.of_int (List.length txouts)) in
		len ^ (serialize_all' txouts)
	;;

	let parse bdata =
		match%bitstring bdata with
		| {|
			value		: 64 : littleendian;
			rest		: -1 : bitstring
		|} ->
			let sclen, rest' = parse_varint rest in
			match%bitstring rest' with
			| {|
				script 		: Uint64.to_int (sclen) * 8 : string;
				rest''		: -1 : bitstring
			|} ->
			let sc = Script.parse script in
			(rest'', Some ({ value= value; script= sc; }))
		| {| _ |} -> (bitstring_of_string "", None)
	;;


	let parse_all data =
		let outlen, rest' = parse_varint data in
		let rec parse_all' n d acc = match n with
		| 0 -> (d, Some (acc))
		| n ->
			let rest, txout = parse d in
			match txout with
			| None -> (bitstring_of_string "", None)
			| Some (txout) -> parse_all' (n-1) rest (txout::acc)
		in parse_all' (Uint64.to_int outlen) rest' []
	;;
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
};;


let parse ?(coinbase=false) data =
	let bdata = bitstring_of_string data in
	match%bitstring bdata with
	| {|
		version		: 32 : littleendian;
		rest		: -1 : bitstring
	|} ->
		let rest', txin = In.parse_all ~coinbase:coinbase rest in
		let rest'', txout = Out.parse_all rest' in
		match (txin, txout) with
		| None, None -> ("", None)
		| None, Some (txout) -> ("", None)
		| Some (txout), None -> ("", None)
		| Some (txin), Some (txout) ->
			let bdata = rest'' in
			match%bitstring bdata with
			| {|
				locktime	: 32 : littleendian;
				rest		: -1 : bitstring
			|} ->
				let rest''' = string_of_bitstring rest in
				let txlen = (Bytes.length data) - (Bytes.length rest''') in
				let txhash = Hash.of_bin (Hash.hash256 (Bytes.sub data 0 txlen)) in
				(rest''', Some ({
					hash	= txhash;
					version	= version;
					txin	= List.rev txin;
					txout	= List.rev txout;
					locktime= Uint32.of_int32 locktime;
				}))
			| {| _ |} -> ("", None)
	| {| _ |} -> ("", None)
;;




let print tx =
	Printf.printf ""; ()
;;

let serialize tx =
	let res = Bitstring.string_of_bitstring ([%bitstring {| tx.version : 32 : littleendian |}]) in
	let res = res ^ (In.serialize_all tx.txin) ^ (Out.serialize_all tx.txout) in
	let ltime = Bitstring.string_of_bitstring ([%bitstring {| Uint32.to_int32 tx.locktime : 32 : littleendian |}]) in
	res ^ ltime
;;

let rec serialize_all txs = match txs with
| [] -> ""
| tx::txs' -> (serialize tx) ^ (serialize_all txs')
;;

let parse_all data ntx =
	let rec parse_all' n d acc =
		(*Printf.printf "Loop! %d\n%!" n;*)

	match n with
	| 0 -> (*Printf.printf "End! %d\n%!" n;*) Some (acc)
	| n ->
		let rest, tx = if n = ntx then parse d ~coinbase:true else parse d in
		match tx with
		| None -> None
		| Some (mtx) ->
			let ser = serialize mtx in
			let dlen = (String.length d) in
			let rlen = (String.length rest) in
			let subs = String.sub d 0 (dlen - rlen) in

			if (subs <> ser) then (
				Printf.printf "Wrong!\n%!";
				Printf.printf "Original: %s\n%!" (Hash.print_bin d);
				Printf.printf "New: %s\n%!" (Hash.print_bin (serialize mtx));
				None
			) else (
				parse_all' (n-1) rest (mtx::acc)
			)
	in
	parse_all' ntx data []
;;