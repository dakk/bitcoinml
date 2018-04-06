open Script;;

module type Script_template_output = sig
  type t 

  val encode: t -> Script.t
  val decode: Script.t -> t
  val check: Script.t -> bool
end

module type Script_template_input = sig
  type t 

  val encode: t -> Script.t
  val decode: Script.t -> t
  val check: Script.t -> bool
end

module Make_template (ITemplate: Script_template_input) (OTemplate: Script_template_output) = struct 
  type t_inp = ITemplate;;
  type t_out = OTemplate;;

  let check_input s = ITemplate.check s;;
  let check_output s = OTemplate.check s;;
end

