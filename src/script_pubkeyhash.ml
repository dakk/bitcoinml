open Script;;
open Address;;


module Input = struct 
  type t = {
    signature: string;
    pubkey: string
  };;

  let check v = 
    (* TODO check if [0] it's a canonical DER signaure (bip66) and [1] is a canonicalpubkey*) 
    match fst v with 
    | OP_DATA (n, sign) :: OP_DATA (n', pk) :: [] -> true
    | _ -> false
  ;;

  let encode v = Script.of_opcodes [
    OP_DATA (String.length v.signature, v.signature);
    OP_DATA (String.length v.pubkey, v.pubkey)
  ];;

  let decode v = 
    match fst v with
    | OP_DATA (n, sign) :: OP_DATA (n', pk) :: [] -> { signature= sign; pubkey= pk }
    | _ -> { signature= ""; pubkey= "" }
  ;;
end

module Output = struct 
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

  let spendable_by s prefix = decode s |> Address.of_pubhash prefix.pubkeyhash;; 
end

(*
module Script_pubkeyhash = Script_template.Make_template
  (Input)
  (Output)  
;;
*)