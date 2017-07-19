open Bitcoinml;;
open Stdint;;
open OUnit2;;
open Hex;;

let base58_encode_check_test octx =
	let adr = Hex.to_string (`Hex "00010966776006953D5567439E5E39F86A0D273BEED61967F6") in
	let adrb58 = Base58.encode_check adr in
	assert_equal adrb58 "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
;;

let varint_parse_test bl res octx =
	let v1 = Varint.parse_varint (Bitstring.bitstring_of_string (Hex.to_string bl)) in
	assert_equal (fst v1) (Uint64.of_int res)
;;

let varint_serialize_test bl res octx =
	let l = Varint.bitstring_of_varint (Int64.of_int res) in
	assert_equal (Bitstring.string_of_bitstring l) (Hex.to_string bl)
;;

let script_parse_test sc scdec octx =
	let s = Script.parse (Hex.to_string sc) in
	assert_equal (fst s) (fst scdec)
;;

let script_serialize_test sc scdec octx =
	let s = Script.serialize scdec in
	assert_equal (Hex.to_string sc) s
;;

let script_is_spendable_test sc octx =
	assert_equal (Script.is_spendable sc) true
;;

let script_spendable_by_test sc adr octx =
	let s = Script.spendable_by sc in
	assert_equal s (Some adr)
;;

let tx_parse_test raw octx =
	let raw', tx = Tx.parse (Hex.to_string raw) in
	match tx with
	| None -> assert_equal true false
	| Some (t) -> assert_equal true true
;;

let suite = "bitcoinml" >::: [
	"base58.encode_check" 	>:: base58_encode_check_test;
	"varint.parse" 			>:: varint_parse_test (`Hex "16") 0x16;
	"varint.parse2" 		>:: varint_parse_test (`Hex "FE32323232") 0x32323232;
	(*"varint.parse3" 		>:: varint_parse_test (`Hex "FF3232323232323232") 0x3232323232323232;*)
	"varint.serialize"		>:: varint_serialize_test (`Hex "16") 0x16;
	"varint.serialize2" 	>:: varint_serialize_test (`Hex "FE32323232") 0x32323232;
	(*"varint.serialize3"	>:: varint_serialize_test (`Hex "FF3232323232323232") 0x3232323232323232;*)
	"script.parse" 			>:: script_parse_test (`Hex "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC")
		([
			Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
			Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
		], 25);
	"script.serialize" 		>:: script_serialize_test (`Hex "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC")
		([
			Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
			Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
		], 25);
	"script.is_spendable"	>:: script_is_spendable_test
		([
			Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
			Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
		], 25);
	"script.spendable_by"	>:: script_spendable_by_test
		([
			Script.OP_DUP; Script.OP_HASH160; Script.OP_DATA (20, Hex.to_string (`Hex "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"));
			Script.OP_EQUALVERIFY; Script.OP_CHECKSIG
		], 25)
		"1DYwPTpZuLjY2qApmJdHaSAuWRvEF5skCN";
		"tx.parse" >:: tx_parse_test (`Hex "01000000017b1eabe0209b1fe794124575ef807057c77ada2138ae4fa8d6c4de0398a14f3f00000000494830450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01ffffffff01f0ca052a010000001976a914cbc20a7664f2f69e5355aa427045bc15e7c6c77288ac00000000")
];;

let () = run_test_tt_main suite;;