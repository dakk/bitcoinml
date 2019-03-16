open Script;;
open Address;;
open Bitstring;;


module Output = struct 
  type t = Hash.t;;
  let name = "wcommitment";;

  let encode wc = 
    Script.of_opcodes [ OP_RETURN (string_of_bitstring [%bitstring {|
		  0x24 : 1*8 : littleendian;
      (Hex.to_string (`Hex "aa21a9ed")) : 4*8 	: string;
      wc : 32 * 8 : string
	  |}] ) ] 
  ;;

  let decode s = 
    match fst s with
    | OP_RETURN (data) :: [] when String.length data >= 38 -> (
      match%bitstring (bitstring_of_string data) with
      | {| 
        p24 		: 1*8 	: littleendian;
        header	: 4*8 	: string;
        hash	  : 4*8 	: string;
        rest    : -1    : bitstring
      |} -> 
        if p24 <> 0x24 then ""
        else if "aa21a9ed" <> (Hex.of_string header |> Hex.show) then ""
        else Hash.of_bin hash)
    | _ -> ""
  ;;

  let check s = decode s <> "";;

  let spendable_by s prefix = "";; 
end

module Script_witnesspubkeyhash = Script_template.Make_template (Script_template.EmptyInput) (Output);;