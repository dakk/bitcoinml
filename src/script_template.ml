
module EmptyInput = struct 
  type t = string;;
  let check v = false;;
  let encode v = Script.empty;;
  let decode v = "";;
end


module type Output = sig
  type t 

  val name: string
  val encode: t -> Script.t
  val decode: Script.t -> t
  val check: Script.t -> bool
  val spendable_by: Script.t -> Address.prefix -> Address.t
end

module type Input = sig
  type t 

  val encode: t -> Script.t
  val decode: Script.t -> t
  val check: Script.t -> bool
end


module Make_template (ITemplate: Input) (OTemplate: Output) = struct 
  type t_inp = ITemplate;;
  type t_out = OTemplate;;

  let input = ITemplate;;
  let output = OTemplate;;
end
