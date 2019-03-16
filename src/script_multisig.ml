open Script;;

let op_int_base = Script.opcode_to_hex OP_RESERVED;;

module Input = struct 
  type t = int;;
  let check v = true;;
  let encode v = Script.empty;;
  let decode v = 0;;
end

module Output = struct 
  type t = {
    m: int;
    pubkeys: string list;
  };;
  let name = "p2ms";;

  let check v = true;;
  
  let encode v = Script.of_opcodes [ OP_CHECKMULTISIG ];;

  let decode s = {
    m= 0;
    pubkeys= []
  };;

  let spendable_by s prefix = "";;
end


module Script_multisig = Script_template.Make_template (Script_template.EmptyInput) (Output);;
