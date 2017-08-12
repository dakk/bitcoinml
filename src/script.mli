module SStack : sig
	type t = int Stack.t

	val create      : unit -> t
	val pop         : t -> int
	val top         : t -> int
	val push        : int -> t -> unit
	val push_data   : bytes -> t -> unit
end


type opcode =
	| OP_COINBASE of bytes

	(* Constants *)
	| OP_0
	| OP_FALSE
	| OP_DATA of int * bytes
	| OP_PUSHDATA1 of int * bytes
	| OP_PUSHDATA2 of int * int * bytes
	| OP_PUSHDATA4 of int * int * int * int * bytes
	| OP_1NEGATE
	| OP_1
	| OP_TRUE
	| OP_2
	| OP_3
	| OP_4
	| OP_5
	| OP_6
	| OP_7
	| OP_8
	| OP_9
	| OP_10
	| OP_11
	| OP_12
	| OP_13
	| OP_14
	| OP_15
	| OP_16

	(* Flow *)
	| OP_IF
	| OP_NOTIF
	| OP_ELSE
	| OP_ENDIF
	| OP_VERIFY
	| OP_RETURN of bytes

	(* Stack *)
	| OP_TOALTSTACK
	| OP_FROMALTSTACK
	| OP_IFDUP
	| OP_DEPTH
	| OP_DROP
	| OP_DUP
	| OP_NIP
	| OP_OVER
	| OP_PICK
	| OP_ROLL
	| OP_ROT
	| OP_SWAP
	| OP_TUCK
	| OP_2DROP
	| OP_2DUP
	| OP_3DUP
	| OP_2OVER
	| OP_2ROT
	| OP_2SWAP

	(* Splice *)
	| OP_CAT
	| OP_SUBSTR
	| OP_LEFT
	| OP_RIGHT
	| OP_SIZE

	(* Bitwise logic *)
	| OP_INVERT
	| OP_AND
	| OP_OR
	| OP_XOR
	| OP_EQUAL
	| OP_EQUALVERIFY

	(* Arithmetic*)
	| OP_1ADD
	| OP_1SUB
	| OP_2MUL
	| OP_2DIV
	| OP_NEGATE
	| OP_ABS
	| OP_NOT
	| OP_0NOTEQUAL
	| OP_ADD
	| OP_SUB
	| OP_MUL
	| OP_DIV
	| OP_MOD
	| OP_LSHIFT
	| OP_RSHIFT
	| OP_BOOLAND
	| OP_BOOLOR
	| OP_NUMEQUAL
	| OP_NUMEQUALVERIFY
	| OP_NUMNOTEQUAL
	| OP_LESSTHAN
	| OP_GREATERTHAN
	| OP_LESSTHANOREQUAL
	| OP_GREATERTHANOREQUAL
	| OP_MIN
	| OP_MAX
	| OP_WITHIN

	(* Crypto *)
	| OP_RIPEMD160
	| OP_SHA1
	| OP_SHA256
	| OP_HASH160
	| OP_HASH256
	| OP_CODESEPARATOR
	| OP_CHECKSIG
	| OP_CHECKSIGVERIFY
	| OP_CHECKMULTISIG
	| OP_CHECKMULTISIGVERIFY

	(* Lock time *)
	| OP_CHECKLOCKTIMEVERIFY
	| OP_CHECKSEQUENCEVERIFY

	(* Pseudo words *)
	| OP_PUBKEYHASH
	| OP_PUBKEY
	| OP_INVALIDOPCODE

	(* Reserved words*)
	| OP_RESERVED
	| OP_VER
	| OP_VERIF
	| OP_VERNOTIF
	| OP_RESERVED1
	| OP_RESERVED2
	| OP_NOP of int
	[@@deriving sexp]

	
val opcode_to_string    : opcode -> string
val opcode_to_hex       : opcode -> int list
val opcode_of_hex       : bytes -> opcode * bytes

type t = opcode list * int [@@deriving sexp]

val length              : t -> int
val serialize           : t -> bytes
val parse               : bytes -> t
val parse_coinbase      : bytes -> t
val verify              : t -> t -> bool
val to_string           : t -> string

val is_spendable        : t -> bool
val spendable_by        : t -> Address.prefix -> string option

