open Bitcoinml;;
open Stdint;;
open OUnit2;;
open Hex;;

(*
let parse_test prefix pub addr octx =
	assert_equal addr @@ Address.of_pubhash prefix (Hex.to_string pub) 
;;

let serialize_test msg octx =
	let ad = Proto.serialize msg in
	assert_equal address @@ ad
;;
*)

let tlist = [
	(*"proto.parse" >:: address_of_pubhash_test 0x05 (`Hex "010966776006953D5567439E5E39F86A0D273BEE") "31nVrspaydBz8aMpxH9WkS2DuhgqS1fCuG";
  "proto.serialize" >:: address_of_pubhash_test 0x00 (`Hex "010966776006953D5567439E5E39F86A0D273BEE") "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM";
  *)
];;