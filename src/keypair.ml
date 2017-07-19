type t = {
	pub     : string;
	priv    : string;
	address : string;
};;


let generate () =
	{ pub="123"; priv=""; address="1A" }
;;

let from_wif wif =
	{ pub="123"; priv=""; address="1A" }
;;


let sign kp data = "";;

let verify kp signed_data = true;;