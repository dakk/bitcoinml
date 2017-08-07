type t = Hash.t

val of_txs    : Tx.t list -> t
val of_hashes : Hash.t list -> t