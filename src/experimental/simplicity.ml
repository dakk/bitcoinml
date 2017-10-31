type t = 
| Unit
| Iden
| Take of t
| Drop of t
| Injl of t
| Injr of t
| Comp of t * t
| Case of t * t
| Pair of t * t
;;


module Examples = struct
  let not = Comp (
    (Pair (Iden, Unit)),
    (Case (Injr Unit, Injl Unit))
  );;

  let halfadder = Case (
    Drop (Pair (Injl (Unit), Iden)),
    Drop (Pair (Iden, not))
  );;

  let fulladder1 = 
    Comp (
      Pair (Take halfadder, Drop Iden),
      Comp ( 
        Pair (
          Take (Take Iden),
          Comp (
            Pair (Take (Drop Iden), Drop Iden),
            halfadder
          )
        ),
        Pair (
          Case (Injr Unit, Drop (Take Iden)),
          Drop (Drop Iden)
        )
      )
    )
  ;;
end
  
(*let rec eval e = match e with
| Iden -> fun x -> x
| Comp (s, t) -> fun x -> eval t (eval s x)
| Injl (t) -> fun x -> fst (eval t x)
| Injr (t) -> fun x -> snd (eval t x)
| Pair (s, t) -> fun x -> (eval s x, eval t x)
;;*)
  
  