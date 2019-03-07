open Stdint;;

let rec b2l t = match Bytes.length t with 
| 0 -> []
| n ->
  let ch = Bytes.get t 0 |> Char.code in
  ch :: (b2l @@ Bytes.sub t 1 (n-1))
;;