(** Lazy block parser, where only the header is parsed while the transaction
  list parsing is deffered to the future *)
  
type t = {
  header	: Block.Header.t;
  ltxs		: Tx.t list option Lazy.t;
  size		: int
}

val parse				: string -> t option
(** Lazy parse of a block *)

val parse_legacy	: string -> t option
(** Lazy parse of a legacy block *)	

val force					: t -> Block.t option
(** Force a lazy eval *)

val force_option	: t option -> Block.t option
(** Force a lazy eval of an option lazy block *)