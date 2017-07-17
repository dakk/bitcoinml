type t = {
    pub  : string;
    priv : string;
};;


val from_wif        : string -> t 
val from_mnemonic   : string -> t

