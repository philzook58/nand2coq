

(*

1. Basics:

How do you program in Coq? No proof weirdness.

Install Proof General. Sry.
How do you run coq?

use bool as example

Coq-Haskell/Ocaml Analogs

Coq has an overblown syntax as a programming language. Stuff is called weird things.

Inductive ~ datatypes  
Definition ~ definition of functions or type alias. Coq is dependently typed so barrier between types and values is weak
FixPoint ~ recursive calls 


Compute .
Check .


Import



Tuple
Either
Maybe
Unit
Void
Nat
List

map
filter
fold


Useful stuff in the standard lib


Extraction?


 *)

(* Proof General execute lines with Ctrl+C + Enter *)

(* Coq Vernacular  
The interactive mode of coq has many commands. These typically start with capital latters.

About and Print will tell us information about something 

By default Coq imports a prelude. 

Built in data types on load https://coq.inria.fr/library/Coq.Init.Datatypes.html

bool, list, option, sum, prod, nat, unit, Empty_set, comparison, identity


*)

About bool.
About prod.
About list.




(*


There are also some functions available for these data types. more functions are available upon extra importing

*)


Compute andb true false.
Compute app (cons 1 nil) (cons 2 (cons 3 nil)).
Compute 1 + 1.
Compute 2 * 3.
Compute 5 - 2.
(* subtraction is defined to be 0 if it would be negative *)
Print "-".
Compute 3 - 5.

Search ( nat -> nat -> bool).
Compute Nat.eqb 1 2.
Compute Nat.div 6 5. (* rounds down *)

Search list.


(* Print can give you a more complete infromation. For example the code implementing the function *)
Print app.



(* You can get a listing of all entries that involve bool*)
Search bool.


Compute andb true false.

(* 
Coq has a special mechanism to support notations. One that is useful and not on by default is list notation
Starting Notations



 *)
Require Import Lists.List.
Import ListNotations.
Compute app [ 1 ] [ 2 ; 3 ].
Search (list ?m -> list ?m).
SearchRewrite (O + _).
Search list.


About hd.

Compute hd 0 (cons 1 nil).
Compute (tl ([ 1 ; 2 ; 3])).
Compute hd 0 [ 1 ; 2 ; 3]. (* head takes a default value *)
Compute hd 0 [].


(* new global names can be made via the Definition vernacular command *)

Definition double (x : nat) : nat := 2 * x.
Compute double 3.


(* pattern matching  *)
Definition andb (b1 : bool) (b2 : bool) : bool :=
  match b1 with
    | true => b2
    | false => false
  end.

About andb.

(* Recursive definitions need to be declared with Fixpoint  *)


Fixpoint factorial (x : nat) : nat :=
  match x with
   | 0 => 1
   | S n => x * (factorial n)
  end.

Compute factorial 5.


Search list.

About fold_left.
Compute fold_left (fun x y => x + y) [1 ; 2 ; 3] 0.

(* Lambda functions *)


(* new data types are defined via Inductive *)



Require Coq.extraction.Extraction.
Extraction Language Haskell.
Recursive Extraction app.



Extraction "app.hs" app. (* extract to a file *)
(* You can inspect the Haskell equivalent *) 
Extraction app.
Recursive Extraction app.
Print rev'.


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

About app.
(*



    *)

  

Inductive bool : Type :=
  | true : bool
  | false : bool.


(* Record command *)



Compute andb true true.
Compute andb false true.

Check andb.


Inductive prod (A : Type) (B : Type) : Type :=
  | pair : A -> B -> prod A B.

Check pair.
Compute pair nat bool 3 true.

(*
https://coq.inria.fr/refman/proof-engine/vernacular-commands.html
                                                     *) 

About pair.
Search nat.
Search bool.

(*
false: bool
true: bool
andb: bool -> bool -> bool
bool_rect: forall P : bool -> Type, P true -> P false -> forall b : bool, P b
bool_ind: forall P : bool -> Prop, P true -> P false -> forall b : bool, P b
                                                                           bool_rec: forall P : bool -> Set, P true -> P false -> forall b : bool, P b



                                                                                       

                                                              *)

(* 
Setoid


Floating point numbers are rather difficult to reason about precisely. A replacement for some purposes is to use rational numbers
QArith

Maps and Sets


Vectors

Strings

*)

(*
Require Import Coq.Arith.Arith.
Require Import Coq.Bool.Bool.
Require Export Coq.Strings.String.
Require Import Coq.Logic.FunctionalExtensionality.
Require Import Coq.Lists.List.
Import ListNotations.





*)




