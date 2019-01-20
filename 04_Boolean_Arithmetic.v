(* Bvector is a standard library data type. It is a sized vector of bools, least significant bit first
https://coq.inria.fr/library/Coq.Bool.Bvector.html


https://coq.inria.fr/library/Coq.ZArith.BinIntDef.html


*)
Require Import Bvector.
Require Import ZArith.
Local Open Scope Z_scope.
Compute Bnil.
Compute Bcons true 0 Bnil.

Compute 203%Z.
Compute 203%Z + 20%Z.
Compute 2 + 4.
Compute -4.


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