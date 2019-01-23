(* 


https://www.nand2tetris.org/project02    

Bvector is a standard library data type. It is a sized vector of bools, least significant bit first
https://coq.inria.fr/library/Coq.Bool.Bvector.html


https://coq.inria.fr/library/Coq.ZArith.BinIntDef.html

The Def files are the juiciest

*)
Require Import Bvector.
Require Vector.



Compute Bnil.
Compute Bcons true 0 Bnil.



Definition word := Bvector 16.

Definition wzero : word := Bvect_false 16.
Definition wone : word := Bcons true _ (Bvect_false 15).

Compute true && false.
Compute true || false.

Theorem bvadd' (n : nat) (x y : Bvector n)  (carry : bool) : Bvector n.
Proof. intros. induction n. exact Bnil. inversion x. inversion y.
       refine (Bcons (xorb (xorb h h0) carry) n _). exact (IHn H0 H2). Qed.

Print bvadd'.

About f_equal.
About eq_ind.
About eq_rec_r.

Inductive eq (A:Type) (x:A) : A -> Prop :=
  eq_refl : eq A x x.

About eq.
About eq_rect.
About eq_rec.
About eq_ind.
Search False.

About nat_ind.
About nat_rec.

Print nat_ind.
Definition simpsum : nat -> nat := nat_rec (fun _ => nat) 0 (fun n acc => acc + n).
Compute simpsum 4.

Definition simple_id {n : nat} (x : Bvector n) (y : Bvector n): Bvector n :=
  match x with
  | Vector.cons _ a n' xs => Bcons a n' xs
  | Vector.nil _ => Bnil
                 end.

Fixpoint bvadd {n : nat} (x : Bvector n) : Bvector n -> bool -> Bvector n :=
  match x with
  | Vector.nil _ => fun y carry => match y in (Vector.t _ n') return (match n' with
                                                                  | S _ => unit
                                                                  | Z => Bvector Z
                                                              
                                                                end) with
                               | Vector.nil _ => Bnil
                               | Vector.cons _ _ _ _ => tt
                             end
  | Vector.cons _ a n' xs => fun y carry => match y in (Vector.t _ n'') return (match n'' with
                                                                          | S pn => (Bvector pn -> bool -> Bvector pn) -> Bvector n''
                                                                          | Z => unit
                                                                          end) with
                                      | Vector.nil _ => tt
                                      | Vector.cons _ b n''' ys => fun bvxs => Bcons (xorb carry (xorb a b)) n''' (bvxs ys ((a && b) || (b && carry) || (carry && a)) )  
                                      end (bvadd xs)
  end.
Compute [true ; false ; true].
Compute bvadd [false ; true] [true ; false] false.

Definition myadd (x : nat ): nat -> nat.
  refine (fun y => _).
  exact (x + y).
  Defined.

Compute bvadd (Bcons true _ Bnil) (Bcons Bcons false _ Bnil).

Print refine.
bvadd 

  (*

https://coq.inria.fr/refman/addendum/program.html
Look at the passing of equalities
https://stackoverflow.com/questions/31041297/coq-convoy-pattern
Coq convoy apttern
https://stackoverflow.com/questions/41837820/agda-like-programming-in-coq-proof-general

*)

Definition bvadd' := rect2 (fun n _ _ => Bvector n) (Bnil)
                           (fun n v1 v2 res => fun carry =>   )

Definition simple_id' {n : nat} (x : Bvector n) (y : Bvector n): Bvector n :=
  match x as e in (Vector.t _ s) return Bvector s with
  | Vector.cons _ a n' xs => x
  | Vector.nil _ => Bnil
 end.


(* so I could include the equality*)
Fixpoint bvadd' {n : nat} (carry : bool) (x y : Bvector n) : Bvector n :=
  match n with
  | S n' => fun (x0 : Bvector (S n')) => @Vector.caseS bool (fun _ _ => Bvector n') (fun a n'' xs => Bcons a n' xs) n' x0
  | Z => fun _ => Bnil
  end x.

match x in (Vector.t _ n) return Bvector n with
| Vector.cons _ a n' xs => @Vector.caseS bool (fun n'' v => Bvector n') (fun b _ ys => cons (xorb (xorb a b) carry) (bvadd' ((a && b) || (b && carry) || (carry && a)) xs ys)) y
                 
| Vector.nil _ => Bnil
end.


(* This is sucking. Use rect2? Use a fold?
Use list, not vector?
Use tuple based vector?

When I pattern match, I do get that (S n') = n I think, like kind of
I need an explicit n' = n

Fixpoint bvadd' {n : nat} (carry : bool) (x y : Bvector n) : Bvector n := 
match x with
| Vector.cons _ a n' xs => match y with 
                            | Vector.cons _ b _ ys => cons (xorb (xorb a b) carry) (bvadd' ((a && b) || (b && carry) || (carry && a)) xs ys)
                            | Vector.nil _ => Bcons a n' xs
                            end
| Vector.nil _ => Bnil
end.
*)




(*


asssoc = 

    Bcons a n' xs
match y with 
                           | Vector.cons _ b _ ys => Bcons b ys
                           end

Bcons (a xorb b xorb carry) (bvadd' (orb ((orb (andb a b) (andb b carry)) (andb carry a))) xs ys)
match n with
| S n' => match x with
          | Vector.cons _ a n' xs => match y with 
                          | Vector.cons _ b n' ys => cons (a xorb b xorb carry) (bvadd' ((a && b) || (b && carry) || (carry && a)) xs ys)
                          end
          end
| 0 => Bnil
end. 

*)
(*
Require Import ZArith.
Local Open Scope Z_scope.

Compute 203%Z.
Compute 203%Z + 20%Z.
Compute 2 + 4.
Compute -4.
Print zero_word.


Definition Z_to_bv  (x : Z) (n : nat) : Bvector n :=
match x with 
| Z0 => bv0
| Zpos y => (positive_to_bv y n)
| Zneg y => negb (positive_to_bv y n)
end.

Definition Z_to_bv  (x : Z) (n : nat) : Bvector n := 

Definition bv_to_Z  (v : Bvector _) : Z := 

Thereom from_to_bv
Thereom to_from_bv
*)
(*


Numerical Circuits

An important subclass of circuit are those that emulate the behavior of integers

Adders

Build adder circuit and prove that they emulate integers as long as the overflow bit isn't set




-- adder with carry
halfadd :: Circuit (Bool, Bool) (Bool, Bool)
halfadd = fan xor and

-- ugh. inefficient and ugly.
fulladd :: Circuit (Bool, (Bool, Bool)) (Bool,Bool)
fulladd = let a = Snd . Fst -- decontruct the input into pieces. Laborious pattern matching.
          let b = Snd . Snd
          let c = Fst 
          let sc1  = halfadd . (Par a b) -- first set of 
          let s = Fst . sc1
          let c1 = Snd . sc1
          let sc2 = halfadd . (Par s c)
          let s1 = Fst . sc2
          let c2 = Snd . sc2
          Par s1 (Comp or (Par c1 c2))



*)