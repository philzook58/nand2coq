
(*

2. Basic Proofs

Try to be brief.


Curry-Howard
Tuple = And
Either = Or
Void = False
Intuitionistic. No double negation




Raw Dog without tactics 


Tactics.
Theorem
Lemma


Equality.


LTAC?




Goal

*)
(*
https://stackoverflow.com/questions/32682544/is-there-a-minimal-complete-set-of-tactics-in-coq

https://pjreddie.com/coq-tactics/
http://adam.chlipala.net/itp/tactic-reference.html




Automation

auto - automatic proof search
ring - solves polynomials / numbers stuff.
omega - automatic solves simple problems for integers



exact - give exactly the term that fulfills the proof obligations
refine - give proof with holes

intro{s} intro introduces variables. 

reflexivity - When the goal is an obvious equality

simpl - does computational simplification


exists, , symmetry, 
apply, rewrite, revert, destruct and induction. inversion



Tacticals

repeat
try 
;


*)

About prod.

Theorem myfst (a b : Type): (prod a b) ->  a.
Proof.
  exact fst.
Qed.


Search and.
Search exist.
Theorem twoexists : exists x : nat, 2 = x.
Proof.
  exists 2. reflexivity.
Qed.

Theorem twoexists' : exists x : nat, 2 = x.
Proof.
  eauto. 
Qed.

Search nat.

Theorem twoeven : Nat.even 2 = true.
Proof.
  reflexivity.
Qed.

Definition square (x : nat) : nat := x * x.
Compute square 4.

Search nat.



(*
Theorem squareven : forall x : nat, Nat.even (square x) = true.
Proof.
  intro x. induction x. simpl. reflexivity.
  unfold square. 
 *)
(*
Theorem eveneven : forall x y: nat, Nat.even x = true ->  Nat.even y = true -> Nat.even (x + y) = true.
Proof.
  intros x y Hx Hy. induction x. simpl. apply Hy. 
 *)
(*
Theorem eveneven : forall x: nat, Nat.even (x + x) = true.
Proof.
  intros x. induction x. simpl. reflexivity.  simpl.
 *)
Theorem eveneven : forall x: nat, Nat.even x = true -> Nat.even (S x) = false.
Proof.
  intros x. 