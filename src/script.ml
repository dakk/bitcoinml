open Base58;;
open Sexplib;;
open Conv;;
open Conv_helper;;
open Address;;

module Sigver = struct
	type t = string -> string -> bool;;
end

module SStack = struct
    type t = int Stack.t;;

    let create () = Stack.create ();;

    let pop st = Stack.pop st;;

    let top st = Stack.top st;;

    let rec push_data d st = match Bytes.length d with
    | 0 -> ()
    | n ->
        Stack.push (Char.code (Bytes.get d 0)) st;
        push_data (Bytes.sub d 1 (n-1)) st
    ;;

    let push v st = Stack.push v st;;
end


type data = bytes;;

let data_of_sexp b = Hash.to_bin_norev (string_of_sexp b);;
let sexp_of_data b = sexp_of_string (Hash.of_bin_norev b);;

type opcode =
| OP_COINBASE of data
(* Constants *)
| OP_0
| OP_FALSE
| OP_DATA of int * data
| OP_PUSHDATA1 of int *  data
| OP_PUSHDATA2 of int * int * data
| OP_PUSHDATA4 of int * int * int * int * data
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
[@@deriving sexp];;

type t = opcode list * int [@@deriving sexp];;

let opcode_to_string oc = sexp_of_opcode oc |> Sexp.to_string;;

let opcode_to_hex oc =
    let rec data_to_bytearray d = match Bytes.length d with
    | 0 -> []
    | n -> (Char.code @@ Bytes.get d 0) :: data_to_bytearray (Bytes.sub d 1 (n-1))
    in
    match oc with
    | OP_COINBASE (d) -> data_to_bytearray d

    (* Constants *)
    | OP_0 -> [ 0x00 ]
    | OP_FALSE -> [ 0x00 ]
    | OP_DATA (s, data) -> s :: (data_to_bytearray data)
    | OP_PUSHDATA1 (s, data) -> [ 0x4c; s ] @ (data_to_bytearray data)
    | OP_PUSHDATA2 (s1, s2, data) -> [ 0x4d; s1; s2 ] @ (data_to_bytearray data)
    | OP_PUSHDATA4 (s1, s2, s3, s4, data) -> [ 0x4e; s1; s2; s3; s4 ] @ (data_to_bytearray data)
    | OP_1NEGATE -> [ 0x4f ]
    | OP_1 -> [ 0x51 ]
    | OP_TRUE -> [ 0x51 ]
    | OP_2 -> [ 0x52 ]
    | OP_3 -> [ 0x53 ]
    | OP_4 -> [ 0x54 ]
    | OP_5 -> [ 0x55 ]
    | OP_6 -> [ 0x56 ]
    | OP_7 -> [ 0x57 ]
    | OP_8 -> [ 0x58 ]
    | OP_9 -> [ 0x59 ]
    | OP_10 -> [ 0x5a ]
    | OP_11 -> [ 0x5b ]
    | OP_12 -> [ 0x5c ]
    | OP_13 -> [ 0x5d ]
    | OP_14 -> [ 0x5e ]
    | OP_15 -> [ 0x5f ]
    | OP_16 -> [ 0x60 ]

    (* Flow *)
    | OP_IF -> [ 0x63 ]
    | OP_NOTIF -> [ 0x64 ]
    | OP_ELSE -> [ 0x67 ]
    | OP_ENDIF -> [ 0x68 ]
    | OP_VERIFY -> [ 0x69 ]
    | OP_RETURN (data) -> [ 0x6a ] @ (data_to_bytearray data)

    (* Stack *)
    | OP_TOALTSTACK -> [ 0x6b ]
    | OP_FROMALTSTACK -> [ 0x6c ]
    | OP_IFDUP -> [ 0x73 ]
    | OP_DEPTH -> [ 0x74 ]
    | OP_DROP -> [ 0x75 ]
    | OP_DUP -> [ 0x76 ]
    | OP_NIP -> [ 0x77 ]
    | OP_OVER -> [ 0x78 ]
    | OP_PICK -> [ 0x79 ]
    | OP_ROLL -> [ 0x7a ]
    | OP_ROT -> [ 0x7b ]
    | OP_SWAP -> [ 0x7c ]
    | OP_TUCK -> [ 0x7d ]
    | OP_2DROP -> [ 0x6d ]
    | OP_2DUP -> [ 0x6e ]
    | OP_3DUP -> [ 0x6f ]
    | OP_2OVER -> [ 0x70 ]
    | OP_2ROT -> [ 0x71 ]
    | OP_2SWAP -> [ 0x72 ]

    (* Splice *)
    | OP_CAT -> [ 0x7e ]
    | OP_SUBSTR -> [ 0x7f ]
    | OP_LEFT -> [ 0x80 ]
    | OP_RIGHT -> [ 0x81 ]
    | OP_SIZE -> [ 0x82 ]

    (* Bitwise logic *)
    | OP_INVERT -> [ 0x83 ]
    | OP_AND -> [ 0x84 ]
    | OP_OR -> [ 0x85 ]
    | OP_XOR -> [ 0x86 ]
    | OP_EQUAL -> [ 0x87 ]
    | OP_EQUALVERIFY -> [ 0x88 ]

    (* Arithmetic*)
    | OP_1ADD -> [ 0x8b ]
    | OP_1SUB -> [ 0x8c ]
    | OP_2MUL -> [ 0x8d ]
    | OP_2DIV -> [ 0x8e ]
    | OP_NEGATE -> [ 0x8f ]
    | OP_ABS -> [ 0x90 ]
    | OP_NOT -> [ 0x91 ]
    | OP_0NOTEQUAL -> [ 0x92 ]
    | OP_ADD -> [ 0x93 ]
    | OP_SUB -> [ 0x94 ]
    | OP_MUL -> [ 0x95 ]
    | OP_DIV -> [ 0x96 ]
    | OP_MOD -> [ 0x97 ]
    | OP_LSHIFT -> [ 0x98 ]
    | OP_RSHIFT -> [ 0x99 ]
    | OP_BOOLAND -> [ 0x9a ]
    | OP_BOOLOR -> [ 0x9b ]
    | OP_NUMEQUAL -> [ 0x9c ]
    | OP_NUMEQUALVERIFY -> [ 0x9d ]
    | OP_NUMNOTEQUAL -> [ 0x9e ]
    | OP_LESSTHAN -> [ 0x9f ]
    | OP_GREATERTHAN -> [ 0xa0 ]
    | OP_LESSTHANOREQUAL -> [ 0xa1 ]
    | OP_GREATERTHANOREQUAL -> [ 0xa2 ]
    | OP_MIN -> [ 0xa3 ]
    | OP_MAX -> [ 0xa4 ]
    | OP_WITHIN -> [ 0xa5 ]

    (* Crypto *)
    | OP_RIPEMD160 -> [ 0xa6 ]
    | OP_SHA1 -> [ 0xa7 ]
    | OP_SHA256 -> [ 0xa8 ]
    | OP_HASH160 -> [ 0xa9 ]
    | OP_HASH256 -> [ 0xaa ]
    | OP_CODESEPARATOR -> [ 0xab ]
    | OP_CHECKSIG -> [ 0xac ]
    | OP_CHECKSIGVERIFY -> [ 0xad ]
    | OP_CHECKMULTISIG -> [ 0xae ]
    | OP_CHECKMULTISIGVERIFY -> [ 0xaf ]

    (* Lock time *)
    | OP_CHECKLOCKTIMEVERIFY -> [ 0xb1 ]
    | OP_CHECKSEQUENCEVERIFY -> [ 0xb2 ]

    (* Pseudo words *)
    | OP_PUBKEYHASH -> [ 0xfd ]
    | OP_PUBKEY -> [ 0xfe ]
    | OP_INVALIDOPCODE -> [ 0xff ]

    (* Reserved words*)
    | OP_RESERVED -> [ 0x50 ]
    | OP_VER -> [ 0x62 ]
    | OP_VERIF -> [ 0x65 ]
    | OP_VERNOTIF -> [ 0x66 ]
    | OP_RESERVED1 -> [ 0x89 ]
    | OP_RESERVED2 -> [ 0x8a ]
    | OP_NOP (x) -> [ x ]
;;


let opcode_of_hex s =
    let consume_next s =
        match Bytes.length s with
        | 0 -> failwith "Not enough bytes"
        | 1 -> (Bytes.get s 0 |> Char.code, "")
        | n ->
            let c = Bytes.get s 0 |> Char.code in
            let s' = Bytes.sub s 1 @@ n - 1 in
            (c, s')
    in
    let consume_bytes s sizea =
        let sizea = List.fold_left (fun acc x -> acc * 0xFF + x) 0 sizea in
        if sizea > Bytes.length s then
           (Bytes.sub s 0 (Bytes.length s), "")
        else
           (Bytes.sub s 0 sizea, Bytes.sub s sizea ((Bytes.length s) - sizea))
    in
    let c, s' = consume_next s in
    match (Bytes.length s', c) with
    (* Constants *)
    | l, 0x00 -> (OP_0, s')
    | l, x when x >= 0x01 && x <= 0x4b ->
        let d, s'' = consume_bytes s' [x] in
        (OP_DATA (x, d), s'')
    | l, 0x4c when l >= 1 ->
        let c', s'' = consume_next s' in
        let d, s'' = consume_bytes s'' [c'] in
        (OP_PUSHDATA1 (c', d), s'')
    | l, 0x4c when l < 1 ->
        (OP_NOP (0x4c), s')
    | l, 0x4d when l >= 2 ->
        let c', s'' = consume_next s' in
        let c'', s'' = consume_next s'' in
        let d, s'' = consume_bytes s'' [c'; c''] in
        (OP_PUSHDATA2 (c', c'', d), s'')
    | l, 0x4d when l < 2 ->
        (OP_NOP (0x4d), s')
    | l, 0x4e when l >= 4 ->
        let c', s'' = consume_next s' in
        let c'', s'' = consume_next s'' in
        let c''', s'' = consume_next s'' in
        let c'''', s'' = consume_next s'' in
        let d, s'' = consume_bytes s'' [c'; c''; c'''; c''''] in
        (OP_PUSHDATA4 (c', c'', c''', c'''', d), s'')
    | l, 0x4e when l < 4 ->
        (OP_NOP (0x4e), s')
    | l, 0x4f -> (OP_1NEGATE, s')
    | l, 0x51 -> (OP_1, s')
    | l, 0x52 -> (OP_2, s')
    | l, 0x53 -> (OP_3, s')
    | l, 0x54 -> (OP_4, s')
    | l, 0x55 -> (OP_5, s')
    | l, 0x56 -> (OP_6, s')
    | l, 0x57 -> (OP_7, s')
    | l, 0x58 -> (OP_8, s')
    | l, 0x59 -> (OP_9, s')
    | l, 0x5a -> (OP_10, s')
    | l, 0x5b -> (OP_11, s')
    | l, 0x5c -> (OP_12, s')
    | l, 0x5d -> (OP_13, s')
    | l, 0x5e -> (OP_14, s')
    | l, 0x5f -> (OP_15, s')
    | l, 0x60 -> (OP_16, s')

    (* Flow *)
    | l, 0x63 -> (OP_IF, s')
    | l, 0x64 -> (OP_NOTIF, s')
    | l, 0x67 -> (OP_ELSE, s')
    | l, 0x68 -> (OP_ENDIF, s')
    | l, 0x69 -> (OP_VERIFY, s')
    | l, 0x6a -> (OP_RETURN (s'), "")

    (* Stack *)
    | l, 0x6b -> (OP_TOALTSTACK, s')
    | l, 0x6c -> (OP_FROMALTSTACK, s')
    | l, 0x73 -> (OP_IFDUP, s')
    | l, 0x74 -> (OP_DEPTH, s')
    | l, 0x75 -> (OP_DROP, s')
    | l, 0x76 -> (OP_DUP, s')
    | l, 0x77 -> (OP_NIP, s')
    | l, 0x78 -> (OP_OVER, s')
    | l, 0x79 -> (OP_PICK, s')
    | l, 0x7a -> (OP_ROLL, s')
    | l, 0x7b -> (OP_ROT, s')
    | l, 0x7c -> (OP_SWAP, s')
    | l, 0x7d -> (OP_TUCK, s')
    | l, 0x6d -> (OP_2DROP, s')
    | l, 0x6e -> (OP_2DUP, s')
    | l, 0x6f -> (OP_3DUP, s')
    | l, 0x70 -> (OP_2OVER, s')
    | l, 0x71 -> (OP_2ROT, s')
    | l, 0x72 -> (OP_2SWAP, s')

    (* Splice *)
    | l, 0x7e -> (OP_CAT, s')
    | l, 0x7f -> (OP_SUBSTR, s')
    | l, 0x80 -> (OP_LEFT, s')
    | l, 0x81 -> (OP_RIGHT, s')
    | l, 0x82 -> (OP_SIZE, s')

    (* Bitwise logic *)
    | l, 0x83 -> (OP_INVERT, s')
    | l, 0x84 -> (OP_AND, s')
    | l, 0x85 -> (OP_OR, s')
    | l, 0x86 -> (OP_XOR, s')
    | l, 0x87 -> (OP_EQUAL, s')
    | l, 0x88 -> (OP_EQUALVERIFY, s')

    (* Arithmetic*)
    | l, 0x8b -> (OP_1ADD, s')
    | l, 0x8c -> (OP_1SUB, s')
    | l, 0x8d -> (OP_2MUL, s')
    | l, 0x8e -> (OP_2DIV, s')
    | l, 0x8f -> (OP_NEGATE, s')
    | l, 0x90 -> (OP_ABS, s')
    | l, 0x91 -> (OP_NOT, s')
    | l, 0x92 -> (OP_0NOTEQUAL, s')
    | l, 0x93 -> (OP_ADD, s')
    | l, 0x94 -> (OP_SUB, s')
    | l, 0x95 -> (OP_MUL, s')
    | l, 0x96 -> (OP_DIV, s')
    | l, 0x97 -> (OP_MOD, s')
    | l, 0x98 -> (OP_LSHIFT, s')
    | l, 0x99 -> (OP_RSHIFT, s')
    | l, 0x9a -> (OP_BOOLAND, s')
    | l, 0x9b -> (OP_BOOLOR, s')
    | l, 0x9c -> (OP_NUMEQUAL, s')
    | l, 0x9d -> (OP_NUMEQUALVERIFY, s')
    | l, 0x9e -> (OP_NUMNOTEQUAL, s')
    | l, 0x9f -> (OP_LESSTHAN, s')
    | l, 0xa0 -> (OP_GREATERTHAN, s')
    | l, 0xa1 -> (OP_LESSTHANOREQUAL, s')
    | l, 0xa2 -> (OP_GREATERTHANOREQUAL, s')
    | l, 0xa3 -> (OP_MIN, s')
    | l, 0xa4 -> (OP_MAX, s')
    | l, 0xa5 -> (OP_WITHIN, s')

    (* Crypto *)
    | l, 0xa6 -> (OP_RIPEMD160, s')
    | l, 0xa7 -> (OP_SHA1, s')
    | l, 0xa8 -> (OP_SHA256, s')
    | l, 0xa9 -> (OP_HASH160, s')
    | l, 0xaa -> (OP_HASH256, s')
    | l, 0xab -> (OP_CODESEPARATOR, s')
    | l, 0xac -> (OP_CHECKSIG, s')
    | l, 0xad -> (OP_CHECKSIGVERIFY, s')
    | l, 0xae -> (OP_CHECKMULTISIG, s')
    | l, 0xaf -> (OP_CHECKMULTISIGVERIFY, s')

    (* Lock time *)
    | l, 0xb1 -> (OP_CHECKLOCKTIMEVERIFY, s')
    | l, 0xb2 -> (OP_CHECKSEQUENCEVERIFY, s')

    (* Pseudo words *)
    | l, 0xfd -> (OP_PUBKEYHASH, s')
    | l, 0xfe -> (OP_PUBKEY, s')
    | l, 0xff -> (OP_INVALIDOPCODE , s')

    (* Reserved words*)
    | l, 0x50 -> (OP_RESERVED, s')
    | l, 0x62 -> (OP_VER, s')
    | l, 0x65 -> (OP_VERIF, s')
    | l, 0x66 -> (OP_VERNOTIF, s')
    | l, 0x89 -> (OP_RESERVED1, s')

    | l, x when x = 0x61 || (x >= 0xb0 && x <= 0xb9) -> (OP_NOP (x), s')
    | l, x -> (OP_NOP (x), s')

;;

(*
    https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp
    https://en.bitcoin.it/wiki/Script
*)
let rec _eval sigver st altst scr =
    let rec to_endif_or_else st altst scr = match scr with
    | [] -> false
    | op::scr' -> match op with
        | OP_ENDIF -> _eval sigver st altst scr'
        | OP_ELSE -> _eval sigver st altst scr'
        | _ -> to_endif_or_else st altst scr'
    in
    let rec to_endif st altst scr = match scr with
    | [] -> false
    | op::scr' -> match op with
        | OP_ENDIF -> _eval sigver st altst scr'
        | _ -> to_endif st altst scr'
    in
    match scr with
    | [] -> (SStack.pop st) <> 0
    | op::scr' -> match op with
        (* Constants *)
        | OP_0
        | OP_FALSE -> SStack.push 0 st; _eval sigver st altst scr'
        | OP_DATA (s, data) -> SStack.push_data data st; _eval sigver st altst scr'
        | OP_PUSHDATA1 (s, data) -> SStack.push_data data st; _eval sigver st altst scr'
        | OP_PUSHDATA2 (a, b, data) -> SStack.push_data data st; _eval sigver st altst scr'
        | OP_PUSHDATA4 (a, b, c, d, data) -> SStack.push_data data st; _eval sigver st altst scr'
        | OP_1NEGATE -> SStack.push (-1) st; _eval sigver st altst scr'
        | OP_1
        | OP_TRUE -> SStack.push 1 st; _eval sigver st altst scr'
        | OP_2 -> SStack.push 2 st; _eval sigver st altst scr'
        | OP_3 -> SStack.push 3 st; _eval sigver st altst scr'
        | OP_4 -> SStack.push 4 st; _eval sigver st altst scr'
        | OP_5 -> SStack.push 5 st; _eval sigver st altst scr'
        | OP_6 -> SStack.push 6 st; _eval sigver st altst scr'
        | OP_7 -> SStack.push 7 st; _eval sigver st altst scr'
        | OP_8 -> SStack.push 8 st; _eval sigver st altst scr'
        | OP_9 -> SStack.push 9 st; _eval sigver st altst scr'
        | OP_10 -> SStack.push 10 st; _eval sigver st altst scr'
        | OP_11 -> SStack.push 11 st; _eval sigver st altst scr'
        | OP_12 -> SStack.push 12 st; _eval sigver st altst scr'
        | OP_13 -> SStack.push 13 st; _eval sigver st altst scr'
        | OP_14 -> SStack.push 14 st; _eval sigver st altst scr'
        | OP_15 -> SStack.push 15 st; _eval sigver st altst scr'
        | OP_16 -> SStack.push 16 st; _eval sigver st altst scr'

        (* Flow *)
        | OP_IF -> if SStack.pop st <> 0 then _eval sigver st altst scr' else to_endif_or_else st altst scr'
        | OP_NOTIF -> if SStack.pop st = 0 then _eval sigver st altst scr' else to_endif_or_else st altst scr'
        | OP_ELSE -> to_endif st altst scr'
        | OP_ENDIF -> _eval sigver st altst scr'
        | OP_VERIFY -> if SStack.pop st <> 0 then true else false
        | OP_RETURN (data) -> false

        (* Stack *)
        | OP_TOALTSTACK ->
            let ap = SStack.pop st in SStack.push ap altst;
            _eval sigver st altst scr'
        | OP_FROMALTSTACK ->
            let ap = SStack.pop altst in SStack.push ap st;
            _eval sigver st altst scr'
        | OP_IFDUP -> true
        | OP_DEPTH -> true
        | OP_DROP ->
            SStack.pop st |> ignore;
            _eval sigver st altst scr'
        | OP_DUP ->
            let ap = SStack.top st in SStack.push ap st;
            _eval sigver st altst scr'
        | OP_NIP -> true
        | OP_OVER -> true
        | OP_PICK -> true
        | OP_ROLL -> true
        | OP_ROT -> true
        | OP_SWAP -> true
        | OP_TUCK -> true
        | OP_2DROP -> true
        | OP_2DUP -> true
        | OP_3DUP -> true
        | OP_2OVER -> true
        | OP_2ROT -> true
        | OP_2SWAP -> true

        (* Splice *)
        | OP_SIZE -> true

        (* Bitwise logic *)
        | OP_EQUAL ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a = b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_EQUALVERIFY ->
            let a = SStack.top st in
            let b = SStack.top st in
            if a = b then true else false

        (* Arithmetic*)
        | OP_1ADD ->
            let ap = SStack.top st in SStack.push (ap + 1) st;
            _eval sigver st altst scr'
        | OP_1SUB ->
            let ap = SStack.top st in SStack.push (ap - 1) st;
            _eval sigver st altst scr'
        | OP_NEGATE ->
            let ap = SStack.top st in SStack.push (-ap) st;
            _eval sigver st altst scr'
        | OP_ABS ->
            let ap = SStack.top st in SStack.push (abs ap) st;
            _eval sigver st altst scr'
        | OP_NOT ->
            let ap = SStack.top st in
            if ap = 0 then SStack.push 1 st else SStack.push 0 st;
            _eval sigver st altst scr'
        | OP_0NOTEQUAL ->
            let ap = SStack.top st in
            if ap = 0 then SStack.push 0 st else SStack.push 1 st;
            _eval sigver st altst scr'
        | OP_ADD ->
            let a = SStack.top st in
            let b = SStack.top st in
            SStack.push (a + b) st;
            _eval sigver st altst scr'
        | OP_SUB ->
            let a = SStack.top st in
            let b = SStack.top st in
            SStack.push (a - b) st;
            _eval sigver st altst scr'
        | OP_BOOLAND ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a <> 0 && b <> 0 then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_BOOLOR ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a <> 0 || b <> 0 then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_NUMEQUAL ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a = b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_NUMEQUALVERIFY ->
            let a = SStack.top st in
            let b = SStack.top st in
            if a = b then true else false
        | OP_NUMNOTEQUAL ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a <> b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_LESSTHAN ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a < b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_GREATERTHAN ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a > b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_LESSTHANOREQUAL ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a <= b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_GREATERTHANOREQUAL ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a >= b then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'
        | OP_MIN ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a > b then SStack.push b st else SStack.push a st);
            _eval sigver st altst scr'
        | OP_MAX ->
            let a = SStack.top st in
            let b = SStack.top st in
            (if a > b then SStack.push a st else SStack.push b st);
            _eval sigver st altst scr'
        | OP_WITHIN ->
            let x = SStack.top st in
            let min = SStack.top st in
            let max = SStack.top st in
            (if x >= min && x < max then SStack.push 1 st else SStack.push 0 st);
            _eval sigver st altst scr'

        (* Crypto *)
        | OP_RIPEMD160 -> true
        | OP_SHA1 -> true
        | OP_SHA256 -> true
        | OP_HASH160 -> true
        | OP_HASH256 -> true
        | OP_CODESEPARATOR -> true
        | OP_CHECKSIG -> 
            let s = SStack.top st in
            let pk = SStack.top st in
            (*sigver s pk*)
            false

        | OP_CHECKSIGVERIFY -> 
            let s = SStack.top st in
            let pk = SStack.top st in
            (*SStack.push @@ sigver s pk;*)
            _eval sigver st altst scr'
            
        | OP_CHECKMULTISIG -> true
        | OP_CHECKMULTISIGVERIFY -> true

        (* Lock time *)
        | OP_CHECKLOCKTIMEVERIFY -> true
        | OP_CHECKSEQUENCEVERIFY -> true

        (* Pseudo words *)
        | OP_PUBKEYHASH -> true
        | OP_PUBKEY -> true
        | OP_INVALIDOPCODE -> true

        (* Reserved words*)
        | OP_RESERVED -> true
        | OP_VER -> true
        | OP_VERIF -> true
        | OP_VERNOTIF -> true
        | OP_RESERVED1 -> true
        | OP_RESERVED2 -> true

        | OP_NOP (x) -> _eval sigver st altst scr'
        | _ -> false
;;


let to_string scr = sexp_of_t scr |> Sexp.to_string;;


let eval sigver scr =
    let st = SStack.create () in
    let altst = SStack.create () in
    try _eval sigver st altst (fst scr) with _ -> false
;;

let join s1 s2 = ((fst s1) @ (fst s2), (snd s1) + (snd s2));;

let length scr = snd scr;;

let serialize scr =
    let rec serialize' scr acc = match scr with
    | [] -> acc
    | op::scr' ->
        let r = List.fold_right
            (fun x acc' -> (Bytes.make 1 (Char.chr x)) ^ acc')
            (opcode_to_hex op) ""
        in serialize' scr' (acc ^ r)
    in
    let s = serialize' (fst scr) "" in
    match (snd scr, Bytes.length s) with
    | (n, n') when n = n' -> s
    | (n, n') -> Printf.printf "Wrong size %d %d\n%!" n n'; failwith "Wrong serialize size"
;;

let parse s =
    let rec chunkize s acc = match Bytes.length s with
    | 0 -> acc
    | n when n <= 8192 -> acc @ [ OP_COINBASE (s) ]
    | n ->
        let chunk = Bytes.sub s 0 8192 in
        chunkize (Bytes.sub s 8192 @@ n - 8192) (acc @ [ OP_COINBASE (chunk)])
    in
    let rec parse' s acc = match Bytes.length s with
    | 0 -> acc
    | n -> 
        let op, s' = opcode_of_hex s in 
        parse' s' @@ acc @ [op]
    in 
    if String.length s > 50000 then (chunkize s [], String.length s)
    else (parse' s [], String.length s)
;;


let parse_coinbase s = ([ OP_COINBASE (s) ], String.length s);;


let verify sigver s1 s2 = join s1 s2 |> eval sigver;;

let is_spendable scr =
    let rec iss ops = match ops with
    | [] -> true
    | OP_RETURN (data) :: xl' -> false
    | _ :: xl' -> iss xl'
    in iss (fst scr)
;;

(* Check for common pattern: http://bitcoin.stackexchange.com/questions/35456/which-bitcoin-script-forms-should-be-detected-when-tracking-wallet-balance*)
(* https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses *)
let spendable_by scr prefix =
    match fst scr with
    (* P2PH *)
    | OP_DUP :: OP_HASH160 :: OP_DATA (20, pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] ->
        Some (Address.of_pubhash prefix.pubkeyhash pkh)
    (* P2SH *)
    | OP_HASH160 :: OP_DATA (20, pkh) :: OP_EQUAL :: [] ->
        Some (Address.of_pubhash prefix.scripthash pkh)
    (* P2PK *)
    | OP_DATA (n, pkh) :: OP_CHECKSIG :: [] when n = 33 || n = 65 ->
       Some (Address.of_pub prefix.pubkeyhash pkh)
    (* TODO: P2WPKH *)
    | OP_0 :: OP_DATA (20, wpkh) :: [] ->
        Some (Address.Bech32.encode prefix.hrp 0x00 wpkh)
    (* TODO: P2WSH *)
    | OP_0 :: OP_DATA (32, wsh) :: [] ->
        Some (Address.Bech32.encode prefix.hrp 0x00 wsh)
    | _ -> None
;;