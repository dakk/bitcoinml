open Bitcoinml;;
open Stdint;;
open OUnit2;;

let ints_to_bytes il = 
	let (^$) s c = s ^ String.make 1 c in
	List.fold_left (^$) "" (List.map Char.chr il) 
;;

let ints_to_bytes_test octx = 
	ints_to_bytes [ 0x63; 0x69; 0x61; 0x6f ] |>
	assert_equal "ciao"
;;

let base58_encode_check_test octx =
	let adr = ints_to_bytes [ 0x00; 0x01; 0x09; 0x66; 0x77; 0x60; 0x06; 0x95; 0x3D; 
		0x55; 0x67; 0x43; 0x9E; 0x5E; 0x39; 0xF8; 0x6A; 0x0D; 0x27; 
		0x3B; 0xEE; 0xD6; 0x19; 0x67; 0xF6 ] in
	let adrb58 = Base58.encode_check adr in
	assert_equal adrb58 "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
;;

let varint_parse_test bl res octx = 
	let v1 = Varint.parse_varint (Bitstring.bitstring_of_string (ints_to_bytes bl)) in
	assert_equal (fst v1) (Uint64.of_int res)
;;

let varint_serialize_test bl res octx = 
	assert_equal 0 0
;;

let suite =
	"bitcoinml" >::: [
		"helper.ints_to_bytes"	>:: ints_to_bytes_test ;
		"base58.encode_check" 	>:: base58_encode_check_test ;
		"varint.parse" 			>:: varint_parse_test [0x16] 0x16;
		"varint.parse2" 		>:: varint_parse_test [0xFE; 0x32; 0x32; 0x32; 0x32] 0x32323232;
		(*"varint.parse3" 		>:: varint_parse_test [0xFF; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32] 0x3232323232323232;*)
		"varint.serialize"		>:: varint_serialize_test [0x16] 0x16;
		"varint.serialize2" 	>:: varint_serialize_test [0xFE; 0x32; 0x32; 0x32; 0x32] 0x32323232;
		(*"varint.serialize3"	>:: varint_serialize_test [0xFF; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32; 0x32] 0x3232323232323232;*)
];;

let () = run_test_tt_main suite;;