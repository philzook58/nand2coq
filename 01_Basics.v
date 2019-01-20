

(*

1. Basics:


Let's start avoiding the unique parts of coq. The Coq system has the equivlaent of many things you might be used to in a functional programming language like OCaml or Haskell.

First off, part of the coolness of coq is the interactivity of the tools. It is sort of like a REPL on steroids. It is absolutely vital you follow along with an interactive mode in an editor.
There are a couple options. 


CoqIDE comes along with coq.
Proof General for Emacs
VSCoq for VSCode

They all have the ability to step through code via hotkey bindings. You need to learn those.
Proof General execute lines with Ctrl+C + Enter
VSCoq is Ctrl+Alt+ArrowKeys, (Ctrl + Command + ArrowKeys on Mac)

 *)


(*

By default Coq imports a prelude. 

You can find a list of default built in data types here https://coq.inria.fr/library/Coq.Init.Datatypes.html

This includes the following familiar friends:
bool, list, option, sum, prod, nat, unit, Empty_set, comparison, identity

The interactive mode of coq has many commands. These start with capital latters and are called Vernacular commands. The temrinology Vernacular is why coq files are called .v files.

About and Print will tell us information about something.

*)

About bool.
About prod.
About list.

(*

There are also some functions available for these data types. More functions are available upon extra importing

*)


Compute andb true false. (* You can run code by using the Compute vernacular. *)
Compute 1 + 1.
Compute 2 * 3.
Compute 5 - 2. 
Compute 3 - 5. (* subtraction is defined to be 0 if it would be negative *)
Print "-". 
Compute app (cons 1 nil) (cons 2 (cons 3 nil)). (* app is the list append function *)
Print app. (* Print can give you a more complete infromation. For example the code implementing the function *)



(* What is the nat equality operator? I don't know. But I do know what there type signature is, so I can use the Search vernacular to find functions in the context. Search is very powerful and useful *)

Search ( nat -> nat -> bool).
Compute Nat.eqb 1 2.
Compute Nat.div 6 5. (* rounds down *)

(* We can also search for anything that involves list *)
Search list.

(* You can get a listing of all entries that involve bool*)
Search bool.

(* 
Coq has a special mechanism to support notations. One that is useful and not on by default is list notation


Require loads a library whereas Import brings its definitions into scope. Require Import does both.

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
Compute hd 0 [ 1 ; 2 ; 3]. (* head takes a default value to make it total *)
Compute hd 0 [].


(* new names can be made via the Definition vernacular command *)

(* Sometimes coq can infer all the types.
*)
Definition double x : nat := 2 * x.
Compute double 3.

Definition double' x := 2 * x.
Compute double 3.



(*  Defining double using a lambda instead *)
Definition double'' := fun x => 2 * x.

(* The standard in Haskell and ocaml is to allow type signatures to be inferrred or to place them on seperate lines. In Coq, this is not how it goes. The pieces are typed in place.  *)

Inductive rgb : Type :=
  | red : rgb
  | green : rgb
  | blue : rgb.

  (* A pure enumeration type can get by with very little annotation *)
Inductive rgb' :=
  | red'
  | green'
  | blue'.


Inductive mycont : Type := 
  | IsNat (x : nat) 
  | IsBool (b : bool).

(* From a Haskell perspective, type definitions are most similar to GADT syntax *)
Inductive mycont' : Type := 
  | IsNat' : nat -> mycont' 
  | IsBool' : bool -> mycont'.


About rgb'.
About rgb.

(* Inferred parameters are given in braces {}. When using this function, you can chooce to explicity include these arguments or leave them implciit *)

Definition myid {A : Type} x : A := x.
Compute myid 3.
(*  Adding @ makes all arguments explcit *)
Compute @myid nat 3.
(* Particular arguments can be supplied by name *)
Compute myid (A:=nat) 3.

(* The equivalent of a type alias in Haskell 
type NatList = [Nat]
*)
Definition natlist :=  list nat.


Compute pair 1 2.
About pair.
About sum.


(* pattern matching  *)
Definition andb (b1 : bool) (b2 : bool) : bool :=
  match b1 with
    | true => b2
    | false => false
  end.

About andb.

(* Recursive definitions need to be declared with Fixpoint
Pattern matching happens in explicit mathc statements, the equivalent of Haskell "case".
*)


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



About app.
(*



    *)

  
(*
Inductive bool : Type :=
  | true : bool
  | false : bool.
*)

(* Record command *)



Compute andb true true.
Compute andb false true.

Check andb.

(*
The prod type definition looks like this

Inductive prod (A : Type) (B : Type) : Type :=
  | pair : A -> B -> prod A B.
*)


Check pair.
Compute pair nat bool 3 true.

(*
https://coq.inria.fr/refman/proof-engine/vernacular-commands.html
                                                     *) 

About pair.
Search nat.
Search bool.



Section mysection.
  Variable T : Set.
  Variable S : Set.
  Definition myfst () := 
End mysection.


(*

https://stackoverflow.com/questions/29322534/coq-prop-versus-set-in-typen

Prop, Type, Set

*)




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




