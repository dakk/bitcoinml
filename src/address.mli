type prefix = {
  pubkeyhash: int;
  scripthash: int;
}
(** Address prefixes struct*)

val of_pub			: int -> bytes -> string
(** Get the address from a public key *)

val of_pubhash	: int -> bytes -> string
(** Get the address from a public key hash *)