(** Block_lazy is a special representation of Block, where only the header is parsed leaving the 
    transactions data unparsed until the Lazy is forced *)
  
type t = {
  header	: Block.Header.t;
  ltxs		: Tx.t list option Lazy.t;
  size		: int
}

val parse				: ?hex:bool -> string -> t option
(** [parse ~hex:bool data] parses the [data] and returns a [t] option; [data] could be
    a binary data, or an hex human readable string if [~hex] is true *)

val parse_legacy	: ?hex:bool -> string -> t option
(** [parse_legacy ~hex:bool data]; same as [parse] but disable segwit *)	

val force					: t -> Block.t option
(** [force lblock] forces the parsing of a lazy block [lblock], returning a [Block.t] option *)

val force_option	: t option -> Block.t option
(** [force_option lblock]; same as [force] but receive an option [t] as parameter *)
