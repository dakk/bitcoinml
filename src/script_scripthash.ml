open Script;;
open Address;;

module Input = struct 
  type t = int;;
  let check v = true;;
  let encode v = Script.empty;;
  let decode v = 0;;
end

module Output = struct 
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

  let spendable_by s prefix = decode s |> Address.of_pubhash prefix.scripthash;; 
end


(*
module Script_scripthash = Script_template.Make_template
  (Input)
  (Output)  
;;
*)