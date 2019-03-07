type t = Hash.t

val of_txs    : Tx.t list -> t
(** Get the merkle root hash of a transaction list *)

val of_hashes : Hash.t list -> t
(** Get the merkle root hash of an hash list *)