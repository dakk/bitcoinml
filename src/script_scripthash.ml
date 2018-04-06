open Script_templates;;
open Script;;


module Script_scripthash = Make_template
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
    | OP_HASH160 :: OP_DATA (20, sh) :: OP_EQUAL :: [] -> true
    | _ -> false
  ;;

  let encode sh = Script.of_opcodes [ OP_HASH160; OP_DATA (20, sh); OP_EQUAL ];;

  let decode s = 
    match fst s with
    | OP_HASH160 :: OP_DATA (20, sh) :: OP_EQUAL :: [] -> sh
    | _ -> ""
  ;;
end)  
;;