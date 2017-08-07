type t = Hash.t [@@deriving sexp]

val of_txs    : Tx.t list -> t
val of_hashes : Hash.t list -> t
