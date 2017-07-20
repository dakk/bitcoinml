type t = {
	pub     : string;
	priv    : string;
	address : string;
};;


let addr_of_pkh prefix pkh =
  let epkh = (Bytes.make 1 @@ Char.chr prefix) ^ pkh in
  let shrip = Bytes.sub (Hash.dsha256 epkh) 0 4 in
  (epkh ^ shrip) |> Base58.encode_check
;;


let addr_of_pk prefix pk =
	pk |> Hash.sha256 |> Hash.ripemd160 |> addr_of_pkh prefix
;;

let priv_to_wif p =
	let epkh = (Bytes.make 1 @@ Char.chr 0x80) ^ p in
  let shrip = Bytes.sub (Hash.dsha256 epkh) 0 4 in
  (epkh ^ shrip) |> Base58.encode_check
;;

let wif_to_priv w =
	let b' = Base58.encode_check w in
	Bytes.sub b' 1 @@ (Bytes.length b') - 4
;;

let from_priv priv = 
	let { Cstruct.buffer } = Hex.to_cstruct (Hex.of_string priv) in
	let ctx = Secp256k1.Context.create [ Sign; Verify ] in
	match Secp256k1.Secret.of_bytes ctx buffer with
	| None -> None
	| Some (sec) ->
		let pub' = Secp256k1.Public.of_secret ctx sec in
		let pubr = Secp256k1.Public.to_bytes ~compress:false ctx pub' 
			|> Cstruct.of_bigarray 
			|> Hex.of_cstruct
			|> Hex.to_string 
		in
		Some ({ pub=pubr; priv=priv; address=addr_of_pk 0x00 pubr })
;;

let from_wif wif = from_priv @@ wif_to_priv wif;;

let to_wif kp = priv_to_wif kp.priv;;

let sign kp data = "";;

let verify kp signed_data = true;;