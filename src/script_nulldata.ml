open Script;;

module Input = Script_template.EmptyInput;;

module Output = struct 
  type t = Script.data;;
  let name = "nulldata";;

  let check s = 
    match fst s with
    | OP_RETURN (data) :: [] -> true
    | _ -> false
  ;;

  let encode data = Script.of_opcodes [ OP_RETURN (data) ];;

  let decode s = 
    match fst s with
    | OP_RETURN (data) :: [] -> data
    | _ -> ""
  ;;

  let spendable_by s prefix = "";;
end


module Script_nulldata = Script_template.Make_template (Input) (Output);;