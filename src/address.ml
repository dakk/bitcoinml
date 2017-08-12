type prefix = {
  pubkeyhash: int;
  scripthash: int;
};;


let of_pubhash prefix pkh =
  let epkh = (Bytes.make 1 @@ Char.chr prefix) ^ pkh in
  let shrip = Bytes.sub (Hash.dsha256 epkh) 0 4 in
  (epkh ^ shrip) |> Base58.encode_check
;;


let of_pub prefix pk =
	pk |> Hash.sha256 |> Hash.ripemd160 |> of_pubhash prefix
;;
