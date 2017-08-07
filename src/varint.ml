open Stdint;;
open Bitstring;;

let parse_varint bits =
	let parse_tag_byte bits =
		match%bitstring bits with
		| {| tag : 1*8 : string; rest : -1 : bitstring |} -> (Uint8.of_bytes_little_endian tag 0, rest)
	in
	let parse_value bits bytesize =
		match%bitstring bits with
		| {| value : bytesize * 8 : string; rest : -1 : bitstring |} -> 
			match bytesize with
			| 8 -> (Uint64.of_bytes_little_endian value 0, rest)
			| 4 -> (Uint32.to_uint64 (Uint32.of_bytes_little_endian value 0), rest)
			| 2 -> (Uint16.to_uint64 (Uint16.of_bytes_little_endian value 0), rest)
			| _ -> failwith "Varint parse error"
	in
	let tag, rest = parse_tag_byte bits in
		match Uint8.to_int tag with
		| 0xFF -> parse_value rest 8
		| 0xFE -> parse_value rest 4
		| 0xFD -> parse_value rest 2
		| x -> (Uint64.of_uint8 tag, rest)
;;


let bitstring_of_varint i = 
	match i with
	| i when i < 0xFDL -> [%bitstring {| Int64.to_int i : 1*8 : littleendian |}]
	| i when i < 0xFFFFL -> [%bitstring {| 0xFD : 1*8; Int64.to_int i : 2*8 : littleendian |}]
	| i when i < 0xFFFFFFFFL -> [%bitstring {| 0xFE : 1*8; Int64.to_int32 i : 4*8 : littleendian |}]
	| i -> [%bitstring {| 0xFF : 1*8; i : 8*8 : littleendian |}]
;;

