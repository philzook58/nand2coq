
(*

2. Basic Proofs

Try to be brief.


Curry-Howard
Tuple = And
Either = Or
Void = False
Intuitionistic. No double negation


Even more vitally than for programming, you need interactive help from the ocmpiler for theorem proving.


Proof is actually a no-op.

You can use tactics for defining regular functions
https://stackoverflow.com/questions/41837820/agda-like-programming-in-coq-proof-general
Just need to use a period after the type declaration rather than :=
Theorem ~ Fixpoint

Definition three : nat.
exact 3. Defined.

Program automates parts of refinement typing.


*)

(*
It's important to learn the techniques and tactics for proofs,
 But the ultimate goal is to make proofs as simple and automatic as possible. There are some automatic tactics.

*)


(* The Theorems aren't really a seperate part of the language from the programs. It is very interesting. 
What makes the vernacular keywords Theorem and Proof special is how they put coq into proof mode where you can use tactics to write programs

The word "exact" is a tactic that takes a Gallina expression that fulfils the type signature
*)

Theorem double : nat -> nat.
Proof. exact (fun x => 2 * x). Qed.

Compute double 3. (* This didn't work *)

(* It is certainly a stretch to call double a theorem, but we'll see things later that really are more thoerem like. In this case, what we've proven is that it is possible to construct a function with the type nat -> nat. This is not at all unique *)

Theorem weird_double : nat -> nat.
Proof. exact (fun x => 340). Qed.

(* You can also write terms with holes. In languages like Agda and Idris, this is the main way programming and proofs are done, but it seems to be rather unidiomatic coq.

The refine tactic lets you have holes in your expression. Each hole becomes a goal in the proof state that you're going to need to supply later.
*)

Theorem double' : nat -> nat.
Proof. refine (fun x => _). exact (2 * x). Qed.

(* There is a different tactic that introduces variables call intros. This is more idiomatic *)

Theorem double'' : nat -> nat.
Proof. intros x. exact (2 * x). Qed.


Theorem idtheorem: forall (A: Type), A -> A.
Proof. intros. exact X. Qed.

Theorem idtheorem': forall (A: Type), A -> A.
Proof. exact (fun (A:Type) (x:A) => x). Qed.


  (* What is refelxivity?  *)

  (* The exact manner a function is defined vastly changes how similar seeming facts are proved
  For example, depending of whether zero is being added from the left or right is different.
  *)

(* Equality is a propsition with a single constructor eq_refl. I recall finding this very puzzling, and I'm sure I still would if I was probed about it or contemplated hard. Equality is a fundamental, but subtle thing. I find that a rough picture of how typechecking works helps me to understand what is going on. 

x = y is a Prop, which is similar to a type
There may be a way to construct a value of this type or not (depending whether you can actually prove the equality or not).


*)
Theorem eq_3: 3 = 3.
Proof. exact eq_refl. Qed. 

Theorem eq_3': 3 = 3.
Proof. reflexivity. Qed. 

Definition eq_3'' : 3 = 3 := eq_refl.

Theorem andb1 : andb true true = true.
Proof. exact eq_refl. Qed.

Theorem andb1' : andb true true = true.
Proof. reflexivity. Qed.


(*  Reflexivity is a tactic that deals with equality. It also does some simplification and works for equality other than eq *)

Theorem nat_eq: forall (n:nat), n = n.
Proof. intros n. reflexivity. Qed.

Theorem zero_id: forall n, 0 + n = n.
Proof. intros n. reflexivity. Qed.

(* Many proofs are by induction. *)

Theorem zero_id': forall n, n + 0 = n.
Proof. intros n. induction n. reflexivity. simpl. rewrite IHn. reflexivity. Qed.

Theorem zero_id'': forall n, n + 0 = n.
Proof. auto. Qed.

(* We can actually see the proof derived using Print *)
Print zero_id''.

(* the theorem eq_sym and plus_n_0 were used. These we in the hint data base *)

(*

auto can also take a search depth parameter

*)

Search nat.
Theorem double_plus_ungood: forall n, 2 * n = n + n.
Proof. intros. simpl. rewrite zero_id'. reflexivity. Qed. 

Theorem double_plus_ungood': forall n, 2 * n = n + n.
Proof. auto. Qed.

Print double_plus_ungood'.

Theorem plus_comm: forall n m,  m + n = n + m.
Proof. auto. intros n m. induction m. auto. simpl. rewrite IHm. auto. Qed. 

SearchRewrite (_ + S _).


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


(* existentails

eauto - is auto plus a little more. 
*)

Theorem exists_3 : exists (n : nat), n = 3.
Proof. exists 3. reflexivity. Qed.

Theorem exists_3' : exists (n : nat), n = 3. 
Proof. eauto. Qed. 





  (*  Tacticals are higher order tactics. They allow chaining of tactics. 
  
  ; sequences tactics. Each subsequent tactic is applied to the goal tree in parallel

  try - tries a tactic, which may fail

  repeat - does the tactic until it stops applying



  + is backtracking it will try the left branch and then go downward until a tactic fails to apply. Then it will back track to this point

  || tries the left tactic and if it doesn't work in all the goals, it will just use the right tactic
  *)



  (*
     LTAC is the Coq tactic scripting language

     idtac
     fail
     fresh




  *)
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
https://www.cs.cornell.edu/courses/cs3110/2018sp/a5/coq-tactics-cheatsheet.html




Automation

auto - automatic proof search
ring - solves polynomials / numbers stuff.
omega - automatic solves simple problems for integers


The tactic auto is able to solve a goal that can be proved using a sequence of intros, apply, assumption, and reflexivity



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