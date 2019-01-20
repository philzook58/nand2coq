(*

3. Combinatorial Circuits


Ok. We can actually talk about Nand 2 Tetris now

show in coq that you can build new definitions of or out of the defintion of nand and prvoe equivalence


Build data structure for circuits. 

What I'm thinking roughly in Haskell:
-- Too painful point free style?
-- BitVectors would be a convenient. A derived notion? Association of tuples is largely irrelevant
-- Generic Data Types. Anything that is a product type is acceptable.
-- (f Bool)? 
-- Records would also be convenient. Stuff like {ram :: Circuit (BV 32) (BV 32), alu ::  }. No this is fine. This is at the Meta Level.
-- Perhaps if we give an isomorphism to Tuple Bools.


type family BitVector where
   BitVector 0 = ()
   BitVector n = (Bool, BitVector n - 1)
-- if we are only working with powers of 2, it is often easier to use
type family TupleTree where
   TupleTree 0 = Bool
   TupleTree n = (TupleTree n - 1, TupleTree n - 1)

-- HOAS as alternative? Circuits do not have higher order functions. Does that not present a problem?

data Circuit a b where
   Nand :: Circuit (Bool,Bool) Bool 
   Dup :: Circuit a (a,a)
   Par :: Circuit a b -> Circuit c d -> Circuit (a,c) (b,d)
   Comp :: Circuit b c -> Circuit a b -> Circuit a c
   -- we don't seem to need these in most of what follows
   Id :: Circuit a a
   Fst :: Circuit (a,b) a
   Snd :: Circuit (a,b) b

-- Drop :: Circuit a () -- Not sure we really want this / it isn't mostly derivable. Fst and Snd let us destroy things. down to 1 element
-- CTrue :: Circuit () Bool -- This may also be derivable . nand (not x) x = true. tautology :: Circuit Bool Bool
-- CFalse :: Circuit () Bool -- This is deriviable from CTrue. not true = false

-- instance Category Circuit


*)

Inductive circuit : Type -> Type -> Type :=
|  Nand : circuit (bool * bool) bool
|  Dup {A : Type} : circuit A (A * A)
|  Par {A B C D : Type} : circuit A B -> circuit C D ->  circuit (A * C) (B * D)
|  Id {A : Type} : circuit A A
|  Comp {A B C: Type} : circuit B C -> circuit A B -> circuit A C
|  Fst {A B : Type} : circuit (A * B) A 
|  Snd {A B : Type} : circuit (A * B) B.

(*
| Collect : circuit a b -> circuit BVector
| Uncollect 

| IsoForward : circuit a b -> Iso b b' -> circuit a b'
| IsoBackward : circuit a b -> iso a a' -> circuit a' b

| IsoCirc : Iso a a' -> circuit a a'
Theh compose to get forward and backward 
Can totally ignore upon interpetation.
Demonstrainte isomrohpism between bitvectors and tuples of bools
But swaps are isos too. No I don't like it.

| nilCircuit : Circuit nil nil
| consbool :: Circuit (BVect 2) (BVect 1)
| ParVect : circuit a (v n1) -> circuit b (bvect n2) -> circuit bvect (n3 + n4) bvect (n1 +  n2)
*)

Definition par {A B C D : Type} (f : A -> B) (g : C -> D) (x : A * C) : (B * D) :=
match x with (pair a b) => pair (f a) (g b) end.

Definition dup {A : Type} (x : A) : (A * A) :=
pair x x.

Definition fan {A B C: Type} (f : A -> B) (g : A -> C) (x : A) : (B * C) :=
(par f g) (dup x).

Definition nandb := fun x y => negb (andb x y).

Require Import Program.
(* This is also in the standard library under Program
Definition compose {A B C: Type} (f : B -> C) (g : A -> B) : A -> C := fun x => f (g x). 
*)
About prod_curry.
About andb.
Check prod_curry andb.
About id.
Compute id 3.
Fixpoint ceval {a b : Type} (circ : circuit a b) : a -> b :=
match circ with
| Nand => (prod_curry nandb)
| Dup => dup
| Par f g => par (ceval f) (ceval g)
| Id => fun x => x
| Comp f g => compose (ceval f) (ceval g)
| Fst => fst
| Snd => snd
end.


(* I need to learn how to automate this all more. 
They can all go through by truth table, but one would hope for more elegance.
Starts to be unacceptable at 16 bit?

destr_bool from Bool is an Ltac for this

The Bool standard library (which is not imported by default) is definitely worth a look. 
 https://coq.inria.fr/library/Coq.Bool.Bool.html
*)

Require Import Bool.

Hint Resolve negb_involutive.
Theorem emulate {A B C : Type} : forall x : A, forall f : circuit B C, forall g : circuit A B, ceval (Comp f g) x = (ceval f) ((ceval g) x).
Proof. auto. Qed.
Hint Resolve emulate.


Definition nandc := Nand.


Theorem nandb_equiv : forall (b1 b2 : bool), ceval nandc (pair b1 b2) = nandb b1 b2.
Proof. auto.  Qed.

Hint Resolve nandb_equiv.
(* one can build not from a nand by tying the inputs together *)

Search (andb ?m ?m).
Hint Rewrite andb_diag.
Print andb_diag.

Definition negc := Comp Nand Dup.
Theorem negc_equiv : forall (b : bool), ceval negc b = negb b.
Proof. destr_bool. Qed. 
   (* intros b. destruct b; reflexivity. Qed. *)

Hint Rewrite negc_equiv.

Search (negb (negb _)).

Theorem dup_test: forall b : bool, dup b = (b , b).
Proof. auto.


Definition andc := Comp negc Nand.
Theorem andc_equiv : forall (b1 b2 : bool), ceval andc (pair b1 b2) = andb b1 b2.
Proof. intros b1 b2. unfold andc. rewrite emulate. rewrite negc_equiv. rewrite nandb_equiv. unfold nandb. rewrite negb_involutive. trivial. Qed.




(* We can define or using De Morgan's law *)
Definition orc := Comp nandc (Par negc negc).
Theorem orb_equiv : forall (b1 b2 : bool), ceval orc (pair b1 b2) = orb b1 b2.
Proof. intros b1 b2. destruct b1; destruct b2; reflexivity.  Qed.

Definition norc := Comp negc orc.
(* We can build a function that always evalautes to true by nanding two opposites *)
Definition truec := Comp Nand (Comp (Par negc Id) Dup).
Theorem truec_equiv : forall (b : bool), ceval truec b = true.
Proof. destr_bool. Qed.

Definition widen {a b : Type} (c : circuit a b) : circuit (a * a) (b * b) := Par c c.


Definition muxc : circuit ((bool * bool) * bool) bool := 
  Comp orc (Comp (widen andc) (Comp (Par (Par Fst Id) (Par Snd negc)) Dup)).

Theorem muxc_equiv : forall (s a b : bool), ceval muxc (pair (pair a b) s) = if s then a else b.
Proof. intros s a b. destruct s; destruct a; destruct b; reflexivity.  Qed.




(*
Proving catgeorical equivalences
ceval (Comp Fst Dup) = ceval Id 
etc
*)

(*

-- Circuits are boolean functions
-- Any functions (->) are Meta language functions. They are circuit Macros, if you like. 


nand = Nand
not = Comp Nand Dup
or :: Circuit (Bool, Bool) Bool
or = Comp not (Comp Nand (Par not not))
-- partially applied or can be nice for cleaning up syntax
or' :: Circuit a Bool -> Circuit a Bool -> Circuit a Bool
or' f g = Comp or (Par f g)
or'' :: Circuit a (Bool, Bool) -> Circuit a Bool
or'' f = Comp or f 
tautology = Comp Nand (Comp (Par not Id) Dup)

double :: Circuit a b -> Circuit (a,a) (b,b)
double f = Par f f


fan f g = Comp (Par f g) Dup
first f = Par f Id
second g = Par Id g


-- parallel on entire block
or2 = double or
or4 = double or2
or8 = double or4
or16 = double or8 

-- For building circuits that nand many things together to a single output
tree :: Circuit (a,a) a -> Circuit ((a,a),(a,a)) a
tree f = Comp f (Par f f)


or4' = tree or 
or8' = tree or4'
or16' = tree or8'




xor = yada yada
multiplex



eval :: Circuit a b -> a -> b
eval Nand = uncurry nand
eval (Comp g f) = (eval g) . (eval f)
eval (Dup x y) = \x -> (x,x) 
eval (Par f g) = (eval f) *** (eval g)


Alternative - Give pins names. Yikes.


Prove that (eval or) (x,y) == coq_or (x,y) 
and all other proofs


-- There ought to be a very simple LTAC for these. also auto will probably work
-- still, good exercises
-- They are all provable by brute force / truth table


Verilog code gen?
One difficulty would be fresh name generation I think. And that isn't so bad.
If we want higher level verilog output (using word level operators and switch statements and stuff), Have a couple of DSLs. use asserts in the verilog to make sure they are equivalent.
(i.e. that our code gen works.) 


*)