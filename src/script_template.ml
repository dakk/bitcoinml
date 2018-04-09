open Script;;

module type Output = sig
  type t 

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

(*
module Make_template (ITemplate: Input) (OTemplate: Output) = struct 
  type t_inp = ITemplate;;
  type t_out = OTemplate;;

  let check_input s = ITemplate.check s;;
  let check_output s = OTemplate.check s;;
  let spendable_by s = OTemplate.spendable_by s;;
end
*)