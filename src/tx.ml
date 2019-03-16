open Stdint;;
open Bitstring;;
open Varint;;

let amount_check ?(max_money=2100000000000000L)  am = am >= Int64.zero && am <= max_money;;

module In = struct
	type t = {
		out_hash			: string;
		out_n					: uint32;
		script				: Script.t;
		witness_script: Script.data list option;
		sequence			: uint32;
	};;

	let has_witness txin = match txin.witness_script with
	| None -> false
	| Some (wl) -> true
	;;

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
					witness_script= None;
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

	let is_spendable txout = Script_verify.is_spendable txout.script;;

	let spendable_by txout prefix = Script_verify.spendable_by txout.script prefix;;

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
			(match amount_check value with
			| true -> (rest'', Some ({ value= value; script= sc; }))
			| false -> (rest'', None))
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

module Witness = struct 
	type t = {
		hash		: Hash.t;
		marker	: int;
		flag		: int;
		size		: int;
	}

	let rec serialize_fields ins = match ins with 
	| [] -> ""
	| i :: ins' -> match i.In.witness_script with
		| None -> serialize_fields ins'
		| Some (ws) ->
			let rec serws ws' = match ws' with
			| [] -> ""
			| si :: ws'' ->
				(string_of_bitstring @@ Varint.bitstring_of_varint (Int64.of_int (String.length si)))
				^ si 
				^ serws ws''
			in
			(string_of_bitstring @@ Varint.bitstring_of_varint (Int64.of_int (List.length ws)))
			^	serws ws
			^ serialize_fields ins'
	;;

	let parse_fields data n = 
		let rec pfields d n acc =
			let parse_field d =
				let wflen, rest' = parse_varint d in

				let rec pfs rest' n acc = match n with
				| 0 -> (rest', List.rev acc)
				| n ->
					let wslen, rest' = parse_varint rest' in
					match%bitstring rest' with
					| {|
						stitem : Uint64.to_int wslen * 8 : bitstring;
						rest'' : -1 : bitstring
					|} ->
						pfs rest'' (n-1) ((string_of_bitstring stitem) :: acc)
				in pfs rest' (Uint64.to_int wflen) []
			in 
			match n with
			| 0 -> (d, acc)
			| n ->
				let rest, items = parse_field d in
				pfields rest (n-1) (acc @ [items]);
		in 
			let rest, litems = pfields data n [] in
			(rest, Some (litems));;
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
};;

let serialize_legacy ?(hex=false) tx =
	let bdata = Bitstring.string_of_bitstring ([%bitstring {| tx.version : 32 : littleendian |}]) 
	^ (In.serialize_all tx.txin) 
	^ (Out.serialize_all tx.txout)
	^ Bitstring.string_of_bitstring ([%bitstring {| Uint32.to_int32 tx.locktime : 32 : littleendian |}]) in
	if hex then Hex.of_string bdata |> Hex.show else bdata
;;

let serialize ?(hex=false) tx = match tx.witness with
| None -> serialize_legacy ~hex:hex tx
| Some (w) -> 
	let bdata = Bitstring.string_of_bitstring ([%bitstring {| tx.version : 32 : littleendian |}])
	^ Bitstring.string_of_bitstring ([%bitstring {| w.marker : 8 : littleendian |}])
	^ Bitstring.string_of_bitstring ([%bitstring {| w.flag : 8 : littleendian |}])
	^ (In.serialize_all tx.txin) 
	^ (Out.serialize_all tx.txout)
	^ (Witness.serialize_fields tx.txin)
	^ Bitstring.string_of_bitstring ([%bitstring {| Uint32.to_int32 tx.locktime : 32 : littleendian |}]) in
	if hex then Hex.of_string bdata |> Hex.show else bdata
;;

let parse_legacy ?(coinbase=false) ?(hex=false) data =
	let bdata = bitstring_of_string (if hex then Hex.to_string @@ `Hex data else data) in
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
				let txlen = (String.length data) - (String.length rest''') in
				let txhash = Hash.of_bin (Hash.hash256 (String.sub data 0 txlen)) in
				(rest''', Some ({
					hash	= txhash;
					version	= version;
					txin	= List.rev txin;
					txout	= List.rev txout;
					locktime= Uint32.of_int32 locktime;
					size= txlen;
					vsize= txlen;
					witness= None;
				}))
			| {| _ |} -> ("", None)
;;

let parse ?(coinbase=false) ?(hex=false) data = match coinbase with
| true -> parse_legacy ~coinbase:true ~hex:hex data
| false -> 
	let bdata = bitstring_of_string (if hex then Hex.to_string @@ `Hex data else data) in
	match%bitstring bdata with
	| {|
		version		: 32 : littleendian;
		marker		: 8 : littleendian;
		flag			: 8 : littleendian;
		rest			: -1: bitstring
	|} ->
		if marker <> 0x00 || flag <> 0x01 then parse_legacy ~coinbase:false ~hex:hex data
		else 
			let rest', txin = In.parse_all rest in
			let rest'', txout = Out.parse_all rest' in
			match (txin, txout) with
			| None, None -> ("", None)
			| None, Some (txout) -> ("", None)
			| Some (txout), None -> ("", None)
			| Some (txin), Some (txout) ->
				let rest''', fields = Witness.parse_fields rest'' @@ List.length txin in 
				match fields with | None -> ("", None) | Some (fields') ->
				let witlen = (String.length @@ string_of_bitstring rest'') - (String.length @@ string_of_bitstring rest''') + 2 in
				match%bitstring rest''' with
				| {|
					locktime	: 32 : littleendian;
					rest		: -1 : bitstring
				|} -> 
					let rest''' = string_of_bitstring rest in
					let txlen = (String.length data) - (String.length rest''') in
					let vsize = int_of_float @@ ceil ((3. *. float_of_int (txlen - witlen) +. float_of_int txlen) /. 4.) in
					let withash = Hash.of_bin (Hash.hash256 (String.sub data 0 txlen)) in
					let txhash = Hash.of_bin @@ Hash.dsha256 (serialize_legacy ({
						hash= "";
						version	= version;
						txin	= txin;
						txout	= List.rev txout;
						locktime= Uint32.of_int32 locktime;
						size= txlen - witlen;
						vsize= vsize;
						witness= None
						})) in
					let txin' = List.mapi (fun j tin -> { tin with In.witness_script= Some (List.nth fields' j) }) (List.rev txin) in
					(rest''', Some ({
						hash= txhash;
						version	= version;
						txin	= txin';
						txout	= List.rev txout;
						locktime= Uint32.of_int32 locktime;
						size= txlen - witlen;
						vsize= vsize;
						witness= Some ({ 
							hash= withash;
							marker= marker;
							flag= flag;
							size= witlen;
						});
					}))
				| {| _ |} -> ("", None)
;;





let rec serialize_all txs = match txs with
| [] -> ""
| tx::txs' -> (serialize tx) ^ (serialize_all txs')
;;

let rec serialize_all_legacy txs = match txs with
| [] -> ""
| tx::txs' -> (serialize_legacy tx) ^ (serialize_all_legacy txs')
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

			if (subs <> ser) then None else parse_all' (n-1) rest (mtx::acc)
	in
	parse_all' ntx data []
;;


let parse_all_legacy data ntx =
	let rec parse_all' n d acc =
		(*Printf.printf "Loop! %d\n%!" n;*)

	match n with
	| 0 -> (*Printf.printf "End! %d\n%!" n;*) Some (acc)
	| n ->
		let rest, tx = if n = ntx then parse_legacy d ~coinbase:true else parse_legacy d in
		match tx with
		| None -> None
		| Some (mtx) ->
			let ser = serialize_legacy mtx in
			let dlen = (String.length d) in
			let rlen = (String.length rest) in
			let subs = String.sub d 0 (dlen - rlen) in

			if (subs <> ser) then None else parse_all' (n-1) rest (mtx::acc)
	in
	parse_all' ntx data []
;;



let is_witness tx = match tx.witness with
| None -> false
| Some (w) -> true
;;


let is_coinbase tx = 
	if List.length tx.txin <> 1 then false
	else
		match (List.nth tx.txin 0) with
		| i when i.out_hash = Hash.zero && i.out_n = (Uint32.sub Uint32.zero Uint32.one) -> (match i.script with
			| ([OP_COINBASE (s)], l) when l >= 2 && l <= 100 -> true
			| _ -> false)
		| _ -> false
;;
