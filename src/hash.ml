open Cryptokit;;

type t = string;;
type hash = t;;
type b = string;;


(* Helper *)
let reverse s =
	let rec reverse_string_acc s acc index length =
		if index >= length
		then acc
		else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
	in
	reverse_string_acc s "" 0 (String.length s)
;;


let zero = String.make 64 '0';;


(* Binary to hash *)
let of_bin b =
	let rec of_bin' h =
		let tos c = Printf.sprintf "%02x" (int_of_char c) in
		match String.length h with
		| 0 -> ""
		| n -> (tos h.[0]) ^ of_bin' (String.sub h 1 ((String.length h) - 1))
	in
	of_bin' (reverse b)
;;


(* Binary to hash *)
let of_bin_norev b =
	let rec of_bin' h acc =
		let tos c = Printf.sprintf "%02x" (int_of_char c) in
		match String.length h with
		| 0 -> acc
		| n -> of_bin' (String.sub h 1 ((String.length h) - 1)) @@ acc ^ (tos h.[0])
	in
	of_bin' b ""
;;


(* Hash to binary *)
let to_bin_generic rev h =
	let dict = "0123456789abcdef" in
	let rec to_bin' h acc = match String.length h with
	| 0 -> acc
	| 1 -> failwith "Hash can't have odd size"
	| n ->
		let spl = String.sub h 0 2 in
		let rest = String.sub h 2 @@ n - 2 in
		let s = Char.chr @@ (String.index dict spl.[0]) * 0x10 + (String.index dict spl.[1]) in
		if rev then to_bin' rest @@ (String.make 1 s) ^ acc else to_bin' rest @@ acc ^ (String.make 1 s)
	in to_bin' h ""
;;

let to_bin h = to_bin_generic true h;;

let to_bin_norev h = to_bin_generic false h;;


let to_bigint h =
	let res7 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 0 8))) in
	let result = Big_int.shift_left_big_int res7 (7 * 32) in
	let res6 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 8 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res6 (6 * 32)) in
	let res5 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 16 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res5 (5 * 32)) in
	let res4 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 24 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res4 (4 * 32)) in
	let res3 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 32 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res3 (3 * 32)) in
	let res2 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 40 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res2 (2 * 32)) in
	let res1 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 48 8))) in
	let result = Big_int.or_big_int result (Big_int.shift_left_big_int res1 (1 * 32)) in
	let res0 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 56 8))) in
	Big_int.or_big_int result (Big_int.shift_left_big_int res0 (0 * 32))
;;


let sha1 data = hash_string (Hash.sha1 ()) data;;
let sha256 data = hash_string (Hash.sha256 ()) data;;

let ripemd160 data = hash_string (Hash.ripemd160 ()) data;;
let dsha256 data = sha256 (sha256 data);;

let hash160 data = ripemd160 (sha256 data);;
let hash256 data = dsha256 data;;

let checksum4 data = String.sub (dsha256 data) 0 4;;