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
  let name = "p2wsh";;

  let check s = 
    match fst s with
    | OP_0 :: OP_DATA (32, wsh) :: [] -> true
    | _ -> false
  ;;

  let encode wsh = Script.of_opcodes [ OP_0; OP_DATA (32, wsh) ];;

  let decode s = 
    match fst s with
    | OP_0 :: OP_DATA (32, wsh) :: [] -> wsh
    | _ -> ""
  ;;

  let spendable_by s prefix = decode s |> Address.of_witness prefix.hrp 0x00;;
end


module Script_witnessscripthash = Script_template.Make_template (Input) (Output);;