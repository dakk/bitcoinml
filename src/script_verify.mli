(** Verification of Bitcoin scripts *)

(** Abstract module for verify functions *)
module Sigver : sig
	type t = string -> string -> bool
end

(** Script execution stack *)
module SStack : sig
	type t = int Stack.t

	val create      : unit -> t
	val pop         : t -> int
	val top         : t -> int
	val push        : int -> t -> unit
	val push_data   : string -> t -> unit
end

val verify              : Sigver.t -> Script.t -> Script.t -> bool
(** [verify sigver s1 s2] execute a script [s1] and [s2] using [sigver] *)

val is_spendable        : Script.t -> bool
(** [is_spendable s] returns true if [s] is spendable by a recognized address *)

val spendable_by        : Script.t -> Address.prefix -> string option
(** [spendable_by s prefix] returns the address (using [prefix]) which is able to
		spend the given script *)