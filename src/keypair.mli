type t = {
    pub     : string;
    priv    : string;
    address : string;
};;


val from_wif        : string -> t 

val sign            : t -> bytes -> bytes
val verify          : t -> bytes -> bool

