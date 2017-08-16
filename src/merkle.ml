type t = Hash.t [@@deriving sexp];;

let of_hashes hashl =
	let rec mround hs = match hs with
	| x' :: [] -> [Hash.dsha256 (x' ^ x')]
	| x' :: x'' :: hs' -> Hash.dsha256 (x' ^ x'') :: mround hs'
	| [] -> []
	in
	let rec m hs = match List.length hs with
		| 0 -> Hash.of_bin @@ Hash.dsha256 (Hash.to_bin Hash.zero)
		| 1 -> Hash.of_bin (List.hd hs)
		| n when n > 1 -> m @@ mround hs
	in 
	List.map (fun h -> Hash.to_bin h) hashl |> m
;;

let of_txs txl = List.map (fun tx -> tx.Tx.hash) txl |> of_hashes;;