open Stdint
open Bitstring


type t = {
	version		: int
}



(*
val create				: unit -> t
val set_lock_time	:	t -> uint32 -> t
val set_version		: t -> int -> t
val add_input			: t -> Hash.t -> int -> int -> Script.t -> t
val add_output		: t -> string -> uint32 -> t
val sign					: t -> int -> Keypair.t -> Script.t -> string -> int -> Script.t -> t
val build					: t -> Tx.t
*)

