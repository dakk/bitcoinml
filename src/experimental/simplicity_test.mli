type __ = Obj.t

type unit0 =
| Tt

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

type ('a, 'b) prod =
| Pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

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

val eval : ty -> ty -> term -> tySem -> tySem
