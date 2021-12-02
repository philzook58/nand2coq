(*  https://www.nand2tetris.org/project04  *)
(* valid destination types. Is there a better way to factr this?
All combinations of registers are possible.
*)

Module Comp.
Variant t := ZERO | ONE | NEGONE | D | A | M | 
NOTD | NOTA | NOTM | NEGD | NEGA | NEGM | INCD |
INCA | INCM | DECD | DECA | DECM |
                         ADD_DA | ADD_DM 
                         | SUB_DA | SUB_DM |
                         SUB_AD | SUB_MD | AND_DA
                         | AND_DM | OR_DA | OR_DM.
End Comp.

Module Dest.
Variant t := NULL | M | D | MD | A | AM | AD | AMD.
End Dest.

Module Jump.
Variant t := NULL | JGT | JEQ | JGE | JLT | JNE | JLE | JMP.
End Jump.


Check bool.
Record bv2 := {
 b0 : bool;
 b1 : bool
}.

(* Definition add2 ( x y : bv2) :=  *)
Search (bool -> bool).

Definition neg2 (x : bv2) :=
    {|
    b0 := negb x.(b0);
    b1 := negb x.(b1)
    |}.

Search bool.
Require Import Bool.
Search negb.

Theorem neg2_involutive : forall x, neg2 (neg2 x) = x.
 intro x. destruct x. Search (negb (negb ?x)).
 unfold neg2. simpl. repeat rewrite negb_involutive.
 reflexivity. Qed.

 Definition unop f x := {|
 b0 := f x.(b0);
 b1 := f x.(b1)
|}.

(*
But also I should try smtcoq
There was also that bitvector library from CAV
*)

(*
Theorem lift_unop P Q (p : forall x, P x = Q x) :
  forall z, unop 

Definition binop f x y := {|
    b0 := f x.(b0) y.(b0);
    b1 := f x.(b1) y.(b1)
|}.

(* Lift any proof *)

Theorem lift (forall x y, f x y =  ) ->
  (forall x y, binop f x y = )
*)




Variant insn := 
  | A : bv16 -> insn
  | C : Dest.t -> Comp.t -> Jump.t -> insn.

Record HackState := {
    A : bv16;
    D : bv16;
    PC : bv16;
    mem : bv16 -> bv16
}.

Require Extraction.

Extraction Jump.
Extraction "assembly.ml" Jump Comp Dest.

(*
Require Import Bvector.

(* Module Dest or Section *)

Inductive dest : Type :=
| iNull | iM | iD | iMD | iA | iAM | iAD | iAMD.

Inductive reg : Type :=
| rA | rM | rD.

Inductive MA : Type :=
| M | A.
(* M register is the location of RAM[A] *)
(* A is an addressing register *)
(* D is an actual register *)

Inductive consts : Type :=
| zero | one | negone.

Inductive comp : Type :=
| cconst : consts -> comp (* fill with constant *)
(* unary operations *)
| creg : reg -> comp (* just put the register in the location *)
| cnot : reg -> comp (* not of register *)
| cneg : reg -> comp (* negative  of register *)
| cinc : reg -> comp  (* increment of register *)
| cdec : reg -> comp (* decrement register *)
(* Binary operations always include D *)
| cand : MA -> comp 
| cor : MA -> comp 
| cadd : MA -> comp
| cDsub : MA -> comp (* D - A/M *)
| csubD : MA -> comp. (* A/M - D *)
*)

(*
Inductive jump : Type :=
| JNull | JGT | JEQ | JGE | JLT | JNE | JLE | JMP.


Inductive instr : Type := 
| At : Bvector 15 -> instr
| Comp : dest -> comp -> jump -> instr.


(* start with unbounded ram 
map using association list or 
map using function? https://softwarefoundations.cis.upenn.edu/lf-current/Maps.html

*)
Definition ram := nat -> word


Definition word := Bvector 16.
Record state
Definition state := {rA : word, rD : word , ram : map addressspace word}

Definition aeval (instate : state) (instr : instr) : state := 
match instr with
| At addr =>  instate (* setA state addr *)
| Comp d c j => instate (* fill out the long implentation *)
end. 

(* 

Questions:
Program counter?
Guaranteeing out of bounds memory?

 *)


(* Opcode translation

Definition opcode (i : instr) : word :=
match i with
| At x => ? Bcons true _ x
| Comp d c j => ? Bcons false _ (opcodeDest d ++  opcodeComp c ++ opCodeComp j )
end.


Definition opcodeDest (d : dest) : Bvector ? :=

Definition opcodeComp (c : comp) : Bvector ? :=
Definition opcodeJump (j : ) : Bvector ? :=
 *)



(* Program Counter?  *)
*)