open Script_templates;;
open Script;;


module Script_pubkeyhash = Make_template
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
    | OP_DUP :: OP_HASH160 :: OP_DATA (20, pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> true
    | _ -> false
  ;;

  let encode pkh = Script.of_opcodes [
    OP_DUP; OP_HASH160; OP_DATA (20, pkh); OP_EQUALVERIFY; OP_CHECKSIG
  ];;

  let decode s = 
    match fst s with
    | OP_DUP :: OP_HASH160 :: OP_DATA (20, pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> pkh
    | _ -> ""
  ;;
end)  
;;