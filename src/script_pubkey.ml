open Script;;
open Address;;


module Script_pubkey = Script_template.Make_template
(struct 
  type t = int;;
  let check v = true;;
  let encode v = Script.empty;;
  let decode v = 0;;
end)
(struct 
  type t = string;;

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
end)  
;;