type unit0 = | Tt;;

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b
;;

type ('a, 'b) prod = | Pair of 'a * 'b;;

let fst = function | Pair (x, _) -> x;;

let snd = function | Pair (_, y) -> y;;

type ty =
| Unit
| Sum of ty * ty
| Prod of ty * ty
;;

type term =
| Iden
| Comp of term * term
| Unit
| Injl of term
| Injr of term
| Case of term * term
| Pair0 of term * term
| Take of term
| Drop of term
;;

let rec eval x a =
  match x with
  | Iden -> a
  | Comp (s, t) -> eval t (eval s a)
  | Unit -> Obj.magic Tt
  | Injl (t) -> Obj.magic (Inl (eval t a))
  | Injr (t) -> Obj.magic (Inr (eval t a))
  | Case (s, t) ->
    let Pair (ab, c0) = Obj.magic a in
    (match ab with
     | Inl a1 -> eval s (Obj.magic (Pair (a1, c0)))
     | Inr b0 -> eval t (Obj.magic (Pair (b0, c0))))
  | Pair0 (s, t) ->
    Obj.magic (Pair ((eval s a), (eval t a)))
  | Take (t) -> eval t (fst (Obj.magic a))
  | Drop (t) -> eval t (snd (Obj.magic a))



module Examples = struct
  let two = Sum (Unit, Unit);;
  let not = Comp (
    (Pair0 (Iden, Unit)),
    (Case (Injr Unit, Injl Unit))
  );;

  let halfadder = Case (
    Drop (Pair0 (Injl (Unit), Iden)),
    Drop (Pair0 (Iden, not))
  );;

  let fulladder1 = 
    Comp (
      Pair0 (Take halfadder, Drop Iden),
      Comp ( 
        Pair0 (
          Take (Take Iden),
          Comp (
            Pair0 (Take (Drop Iden), Drop Iden),
            halfadder
          )
        ),
        Pair0 (
          Case (Injr Unit, Drop (Take Iden)),
          Drop (Drop Iden)
        )
      )
    )
  ;;
end