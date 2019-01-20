(*

3. Combinatorial Circuits


Ok. We can actually talk about Nand 2 Tetris now

show in coq that you can build new definitions of or out of the defintion of nand and prvoe equivalence


Build data structure for circuits. 

What I'm thinking roughly in Haskell:
-- Too painful point free style?

data Circuit a b where
   Nand :: Circuit (Bool,Bool) Bool 
   Dup :: Circuit a (a,a)
   Par :: Circuit a b -> Circuit c d -> Circuit (a,c) (b,d)
   Comp :: Circuit b c -> Circuit a b -> Circuit a c
   -- we don't seem to need these in most of what follows
   Id :: Circuit a a
   Fst :: Circuit (a,b) a
   Snd :: Circuit (a,b) b

-- instance Category Circuit

*)

Inductive circuit : Type -> Type -> Type :=
|  Nand : circuit (bool * bool) bool
|  Dup {A : Type} : circuit A (A * A)
|  Par {A B C D : Type} : circuit A B -> circuit C D ->  circuit (A * C) (B * D)
|  Id {A : Type} : circuit A A.



Definition par {A B C D : Type} (f : A -> B) (g : C -> D) (x : A * C) : (B * D) :=
match x with (pair a b) => pair (f a) (g b) end.

Definition dup {A : Type} (x : A) : (A * A) :=
pair x x.

Definition fan {A B C: Type} (f : A -> B) (g : A -> C) (x : A) : (B * C) :=
(par f g) (dup x).

Definition nand := fun x y => negb (andb x y).

About prod_curry.
About andb.
Check prod_curry andb.
About id.
Compute id 3.
Fixpoint ceval {a b : Type} (circ : circuit a b) : a -> b :=
match circ with
| Nand => (prod_curry andb)
| Dup => dup
| Par f g => par (ceval f) (ceval g)
| Id => fun x => x
end.


(*

nand = Nand
not = Comp Nand Dup
or = Comp not (Comp Nand (Par not not))

double :: Circuit a b -> Circuit (a,a) (b,b)
double f = Par f f

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