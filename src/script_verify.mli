
module Sigver : sig
	type t = string -> string -> bool
end

module SStack : sig
	type t = int Stack.t

	val create      : unit -> t
	val pop         : t -> int
	val top         : t -> int
	val push        : int -> t -> unit
	val push_data   : string -> t -> unit
end

val verify              : Sigver.t -> Script.t -> Script.t -> bool
val is_spendable        : Script.t -> bool
val spendable_by        : Script.t -> Address.prefix -> string option