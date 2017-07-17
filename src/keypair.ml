type t = {
    pub  : string;
    priv : string;
};;


let from_wif wif =
  { pub="123"; priv="" }
;;

let from_mnemonic mn =
  { pub="123"; priv="" }  
;;