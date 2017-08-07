type t = Hash.t;;


let of_hashes hashl =
  let to_odd tl = 
    if ((List.length tl) mod 2) = 0 then tl 
    else List.append tl [List.nth tl @@ List.length tl] 
  in
  let rec mround hs = match hs with
  | x' :: x'' :: hs' -> Hash.dsha256 (x' ^ x'') :: mround hs'
  | [] -> []
  in
  let rec m hs = match List.length hs with
    | 1 -> List.hd hs
    | n -> m @@ mround hs
  in hashl |> to_odd |> m
;;

let of_txs txl = List.map (fun tx -> tx.Tx.hash) txl |> of_hashes;;

