type prefix = {
  pubkeyhash: int;
  scripthash: int;
  hrp: string;
};;

module Bech32 = struct
  let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l";;

  let polymod values =
    let generators = [ 0x3b6a57b2; 0x26508e6d; 0x1ea119fa; 0x3d4233dd; 0x2a1462b3 ] in
    0
  ;;


end

let of_pubhash prefix pkh =
  let epkh = (Bytes.make 1 @@ Char.chr prefix) ^ pkh in
  let shrip = Bytes.sub (Hash.dsha256 epkh) 0 4 in
  (epkh ^ shrip) |> Base58.encode_check
;;


let of_pub prefix pk =
	pk |> Hash.sha256 |> Hash.ripemd160 |> of_pubhash prefix
;;


let of_witness hrp witver pk = "";;