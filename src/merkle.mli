type t = Hash.t

val of_txs    : Tx.t list -> t
(** [of_txs txl] returns the merkle tree hash of the [txl] transaction set *)

val of_hashes : Hash.t list -> t
(** [of_hashes hl] returns the merkle tree hash of the [hl] hash list *)
