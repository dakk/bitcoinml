(** From https://blockstream.com/simplicity.pdf by Russell O’Connor *)

Inductive Ty : Set :=
    | Unit : Ty
    | Sum : Ty -> Ty -> Ty
    | Prod : Ty -> Ty -> Ty.

Fixpoint tySem (x : Ty) : Set :=
    match x with
    | Unit => Datatypes.unit
    | Sum a b => tySem a + tySem b
    | Prod a b => tySem a * tySem b
end.

Inductive Term : Ty -> Ty -> Set :=
    | iden : forall {A}, Term A A
    | comp : forall {A B C},
        Term A B -> Term B C -> Term A C
    | unit : forall {A}, Term A Unit
    | injl : forall {A B C},
        Term A B -> Term A (Sum B C)
    | injr : forall {A B C},
        Term A C -> Term A (Sum B C)
    | case : forall {A B C D},
        Term (Prod A C) D -> Term (Prod B C) D ->
        Term (Prod (Sum A B) C) D
    | pair : forall {A B C},
        Term A B -> Term A C -> Term A (Prod B C)
    | take : forall {A B C},
        Term A C -> Term (Prod A B) C
    | drop : forall {A B C},
        Term B C -> Term (Prod A B) C.

Fixpoint eval {A B} (x : Term A B) :
    tySem A -> tySem B :=
    match x in Term A B
    return tySem A -> tySem B with
        | iden => fun a => a
        | comp s t => fun a => eval t (eval s a)
        | unit => fun _ => tt
        | injl t => fun a => inl (eval t a)
        | injr t => fun a => inr (eval t a)
        | case s t => fun p => let (ab, c) := p in
            match ab with
            | inl a => eval s (a, c)
            | inr b => eval t (b, c)
            end
        | pair s t => fun a => (eval s a, eval t a)
        | take t => fun ab => eval t (fst ab)
        | drop t => fun ab => eval t (snd ab)
end.


Extraction Language Ocaml.
Extraction "simplicity_test.ml" Ty tySem Term eval.