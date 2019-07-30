(*  https://www.nand2tetris.org/project04  *)

(* valid destination types. Is there a better way to factr this?
All combinations of registers are possible.
*)
Require Import Bvector.

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
