
(*

2. Basic Proofs

Try to be brief.


Curry-Howard
Tuple = And
Either = Or
Void = False
Intuitionistic. No double negation


Even more vitally than for programming, you need interactive help from the ocmpiler for theorem proving.



*)

(*
It's important to learn the techniques and tactics for proofs,
 But the ultimate goal is to make proofs as simple and automatic as possible. There are some automatic tactics.

*)



Theorem double : nat -> nat.
Proof. exact (fun x => 2 * x). Qed.

Theorem idtheorem: forall (A: Type), A -> A.
Proof. intros. exact X. Qed.

Theorem idtheorem': forall (A: Type), A -> A.
Proof. exact (fun (A:Type) (x:A) => x). Qed.


  (* What is refelxivity?  *)

  (* The exact manner a function is defined vastly changes how similar seeming facts are proved
  For example, depending of whether zero is being added from the left or right is different.
  *)

(* Equality is a propsition with a single constructor eq_refl
*)
Theorem eq_3: 3 = 3.
Proof. exact eq_refl. Qed. 

Definition eq_3' : 3 = 3 := eq_refl.

(*  Reflexivity is a tactic that deals with equality. It also does some simplification and works for equality other than eq *)

Theorem nat_eq: forall (n:nat), n = n.
Proof. intros n.

Theorem zero_id: forall n, 0 + n = n.
Proof. intros n. reflexivity. Qed.

(* Many proofs are by induction. *)

Theorem zero_id': forall n, n + 0 = n.
Proof. intros n. induction n. reflexivity. simpl. rewrite IHn. reflexivity. Qed.

Search nat.
Theorem double_plus_ungood: forall n, 2 * n = n + n.
Proof.  intros. induction n. simpl. reflexivity. Admitted. 



Theorem andb1 : andb true true = true.
Proof. reflexivity. Qed.

Search True.

(* True is a Prop with a single contstructor I. It is similar to unit. *)
Goal True. 
Proof. apply I. Qed.

Goal forall {A B : Prop}, A /\ B -> A.
Proof. exact proj1. Qed.


Goal forall {A B : Prop}, A -> B -> A /\ B.
Proof. intros A B HA HB. split. apply HA. apply HB.  Qed.

  (*  tt : unit *) 
Search unit.



Theorem exists_3 : exists (n : nat), n = 3. 
Proof. exists 3. reflexivity. Qed.



  (*  Tacticals are higher order tactics. They allow chaining of tactics *))
(*

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


(*
Induction principles made by data type definitions.

*)


(* Case matching on impossible things *)
(* Proving anything from False *)
(*   *)

(* https://stackoverflow.com/questions/40695030/how-to-prove-the-arithmetic-equality-3-s-i-j-1-s-3-i-1-s-3
*)

(* Goal vecrnacular. don't have to give it a name*)
Require Import Arith.
Theorem doub (a : nat) : 2 * a = a + a. 
  ring. Qed.

Require Import Omega.
Theorem doub2 (a : nat) : 2 * a = a + a. 
  omega. Qed.


Theorem simp : exists e, e + 1 = 2.
Proof.
 exists 1. simpl. reflexivity. Qed.


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