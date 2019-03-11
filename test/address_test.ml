open Bitcoinml;;
open Stdint;;
open OUnit2;;
open Hex;;

let address_of_pub_test prefix pub addr octx =
	assert_equal addr @@ Address.of_pub prefix (Hex.to_string pub) 
;;

let address_of_pubhash_test prefix pub addr octx =
	assert_equal addr @@ Address.of_pubhash prefix (Hex.to_string pub) 
;;

let address_bech32_test address hrp ver script octx =
	let ad = Address.of_witness hrp ver (Hex.to_string script) in
	assert_equal address @@ ad
;;

let tlist = [
	"address.of_pub" >:: address_of_pub_test 0x0 (`Hex "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6") "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM";
	"address.of_pubhash" >:: address_of_pubhash_test 0x05 (`Hex "010966776006953D5567439E5E39F86A0D273BEE") "31nVrspaydBz8aMpxH9WkS2DuhgqS1fCuG";
	"address.of_pubhash2" >:: address_of_pubhash_test 0x00 (`Hex "010966776006953D5567439E5E39F86A0D273BEE") "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM";

	"address.bech32" >:: address_bech32_test "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7k7grplx" "bc" 1 (`Hex "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6");
	"address.bech322" >:: address_bech32_test "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4" "bc" 0 (`Hex "751e76e8199196d454941c45d1b3a323f1433bd6");
	"address.bech323" >:: address_bech32_test "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3" "bc" 0 (`Hex "1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262");
	(*"address.bech322" >:: address_bech32_test "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7" "tb" 34 (`Hex "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262");	*)
];;