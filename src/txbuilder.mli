type t = {
    signatures  : string list;
}


val sign : t -> Keypair.t -> t