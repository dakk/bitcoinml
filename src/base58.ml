open Big_int;;

let bitcoin_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";;
let big_base = big_int_of_int 58;;

let encode_check data =
	let bytes_to_bigint data =
		let rec btb data acc = match Bytes.length data with
		| 0 -> acc
		| n ->
			let c' = Bytes.get data 0 |> Char.code in
			let rest = Bytes.sub data 1 (n - 1) in
			btb rest @@ add_int_big_int c' (mult_int_big_int 0x100 acc)
		in btb data zero_big_int
	in
	let rec encode x =
		match eq_big_int x zero_big_int with
		| true -> if (Bytes.get data 0 |> Char.code) = 0x00 then "1" else ""
		| false ->
			let (q, index) = quomod_big_int x big_base in
			let ch = String.make 1 bitcoin_alphabet.[int_of_big_int index] in
			(encode q) ^ ch
	in encode @@ bytes_to_bigint data
;;

