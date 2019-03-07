open Bitstring;;
open Block;;
open Varint;;
open Stdint;;

type t = {
  header	: Block.Header.t;
  ltxs		: Tx.t list option Lazy.t;
  size		: int
};;

let parse data = 
  let header = Block.Header.parse (String.sub data 0 80) in
  match header with
  | None -> None
  | Some (header) ->
    let bdata = bitstring_of_string  (String.sub data 80 ((String.length data) - 80)) in
    let txn, rest' = parse_varint bdata in
    let txs = lazy (Tx.parse_all (string_of_bitstring rest') (Uint64.to_int txn)) in
    Some ({ header= header; ltxs= txs; size= String.length data })
;;


let parse_legacy data =
  let header = Block.Header.parse (String.sub data 0 80) in
  match header with
  | None -> None
  | Some (header) ->	
    let bdata = bitstring_of_string  (String.sub data 80 ((String.length data) - 80)) in
    let txn, rest' = parse_varint bdata in
    let txs = lazy (Tx.parse_all_legacy (string_of_bitstring rest') (Uint64.to_int txn)) in
    Some ({ header= header; ltxs= txs; size= String.length data })
;;

let force lb = match Lazy.force lb.ltxs with
| None -> None
| Some (txs) -> Some ({ header= lb.header; txs= List.rev txs; size= lb.size })
;;

let force_option lb = match lb with
| Some (lb') -> force lb'
| None -> None
;;