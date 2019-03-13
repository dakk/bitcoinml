(** An address is an almost readable representation of Bitcoin wallet identifier *)

type prefix = {
  pubkeyhash: int;
  scripthash: int;
  hrp: string;
}
(** Address prefixes structure *)

type t = string
(** Address abstract type *)

(** Handle segwit native bech32 addresses *)
module Bech32 : sig
  val encode : string -> int -> string -> t
  (** [encode hrp witver witprog] encodes [witprog] in bech32 address given the [hrp] and the 
      [witver] witness version *)
end


val of_pub			: int -> string -> t
(** [of_pub prefix pk] encodes the address from a public key [pk] given the network prefix [prefix] *)

val of_pubhash	: int -> string -> t
(** [of_pubhash prefix pkh] encodes the address from a public key hash [pkh] given the network 
    prefix [prefix] *)

val of_witness  : string -> int -> string -> t
(** [of_witness hrp witver witprog] encodes [witprog] in bech32 address given the [hrp] and the 
     [witver] witness version *)