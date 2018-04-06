open Script_templates;;
open Address;;
open Script;;

let op_int_base = Script.OP_RESERVED;;

module Script_multisig = Make_template
(struct 
  type t = int;;
  let check v = true;;
  let encode v = Script.empty;;
  let decode v = 0;;
end)
(struct 
  type t = {
    m: int;
    pubkeys: string list;
  };;

  let check v = true;;
  let encode v = Script.empty;;
  let decode s = {
    m= 0;
    pubkeys= []
  };;

  let spendable_by s prefix = "";;
end)  
;;