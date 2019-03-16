open Script;;
open Address;;

module Input = struct 
  type t = Script.data;;
  let check v = (* TODO check if it's a canonical DER signaure (bip66) *) true;;
  let encode v = Script.of_opcodes [OP_DATA (String.length v, v)];;

  let decode v = 
    match fst v with
    | [OP_DATA (n, v)] -> v
    | _ -> ""
end


module Output = struct 
  type t = string;;
  let name = "p2pk";;

  let check s = 
    match fst s with
    | OP_DATA (n, pk) :: OP_CHECKSIG :: [] when n = 33 || n = 65 -> true
    | _ -> false
  ;;

  let encode pk = Script.of_opcodes [OP_DATA (String.length pk, pk); OP_CHECKSIG];;

  let decode s = 
    match fst s with
    | OP_DATA (n, pk) :: OP_CHECKSIG :: [] when n = 33 || n = 65 -> pk
    | _ -> ""
  ;;

  let spendable_by s prefix = decode s |> Address.of_pub prefix.pubkeyhash;;
end


module Script_pubkey = Script_template.Make_template (Input) (Output);;