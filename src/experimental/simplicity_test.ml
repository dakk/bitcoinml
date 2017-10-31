type __ = Obj.t

type unit0 =
| Tt

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

type ('a, 'b) prod =
| Pair of 'a * 'b

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Pair (x, _) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Pair (_, y) -> y

type ty =
| Unit
| Sum of ty * ty
| Prod of ty * ty

type tySem = __

type term =
| Iden of ty
| Comp of ty * ty * ty * term * term
| Unit0 of ty
| Injl of ty * ty * ty * term
| Injr of ty * ty * ty * term
| Case of ty * ty * ty * ty * term * term
| Pair0 of ty * ty * ty * term * term
| Take of ty * ty * ty * term
| Drop of ty * ty * ty * term

(** val eval : ty -> ty -> term -> tySem -> tySem **)

let rec eval _ _ x a =
  match x with
  | Iden _ -> a
  | Comp (a0, b, c, s, t) -> eval b c t (eval a0 b s a)
  | Unit0 _ -> Obj.magic Tt
  | Injl (a0, b, _, t) -> Obj.magic (Inl (eval a0 b t a))
  | Injr (a0, _, c, t) -> Obj.magic (Inr (eval a0 c t a))
  | Case (a0, b, c, d, s, t) ->
    let Pair (ab, c0) = Obj.magic a in
    (match ab with
     | Inl a1 -> eval (Prod (a0, c)) d s (Obj.magic (Pair (a1, c0)))
     | Inr b0 -> eval (Prod (b, c)) d t (Obj.magic (Pair (b0, c0))))
  | Pair0 (a0, b, c, s, t) ->
    Obj.magic (Pair ((eval a0 b s a), (eval a0 c t a)))
  | Take (a0, _, c, t) -> eval a0 c t (fst (Obj.magic a))
  | Drop (_, b, c, t) -> eval b c t (snd (Obj.magic a))
