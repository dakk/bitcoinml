type prefix = {
  pubkeyhash: int;
  scripthash: int;
}

val of_pub			: int -> bytes -> string
val of_pubhash	: int -> bytes -> string
