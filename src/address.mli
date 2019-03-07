type prefix = {
  pubkeyhash: int;
  scripthash: int;
  hrp: string;
}
(** Address prefixes struct*)

type t = string
(** Address abstract type *)

module Bech32 : sig
  val encode : string -> int -> string -> t
end


val of_pub			: int -> string -> t
(** Get the address from a public key *)

val of_pubhash	: int -> string -> t
(** Get the address from a public key hash *)

val of_witness  : string -> int -> string -> t
(** Get the bech32 address from a witness script *)