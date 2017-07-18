type t = {
    mutable tx  : Tx.t;
};;

let from_tx tx =
    { tx= tx }
;;

let sign txb kp = txb;;
let to_tx txb = txb.tx;;


let add_input txb txhash vout seq prevoutscript = txb;;

let add_output txb scriptpubkey value = txb;;