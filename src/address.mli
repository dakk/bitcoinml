type prefix = {
  pubkeyhash: int;
  scripthash: int;
  hrp: string;
}
(** Address prefixes struct*)

type t = string
(** Address abstract type *)

module Bech32 : sig
  val charset : string 
  val polymod : bytes -> int

  (* val encode : string -> string -> string -> string*)
end


val of_pub			: int -> bytes -> t
(** Get the address from a public key *)

val of_pubhash	: int -> bytes -> t
(** Get the address from a public key hash *)

val of_witness  : string -> int -> bytes -> t
(** Get the bech32 address from a witness script *)