type prefix = {
  pubkeyhash: int;
  scripthash: int;
  hrp: string;
}
(** Address prefixes struct*)


module Bech32 : sig
  val charset : string 
  val polymod : bytes -> int

  (* val encode : string -> string -> string -> string*)
end


val of_pub			: int -> bytes -> string
(** Get the address from a public key *)

val of_pubhash	: int -> bytes -> string
(** Get the address from a public key hash *)

val of_witness  : string -> int -> bytes -> string
(** Get the bech32 address from a witness script *)