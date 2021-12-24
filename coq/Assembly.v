(*  https://www.nand2tetris.org/project04  *)
(* valid destination types. Is there a better way to factr this?
All combinations of registers are possible.
*)

Module Comp.
Variant t := 
  | ZERO | ONE | NEGONE | D | A | M
  | NOTD | NOTA | NOTM | NEGD | NEGA | NEGM | INCD
  | INCA | INCM | DECD | DECA | DECM | ADD_DA | ADD_DM
  | SUB_DA | SUB_DM | SUB_AD | SUB_MD | AND_DA
  | AND_DM | OR_DA | OR_DM.
End Comp.

Module Dest.
Variant t := NULL | M | D | MD | A | AM | AD | AMD.
End Dest.

Module Jump.
Variant t := NULL | JGT | JEQ | JGE | JLT | JNE | JLE | JMP.
End Jump.

Require Import Bvector.
Require Import ZArith.
Open Scope Z_scope.
(*
https://github.com/jasmin-lang/coqword

https://compcert.org/doc/html/compcert.lib.Integers.html
Compcert deagfling with integers
Huh. The ocmpairson type is pretty simuilkar to jump
*)
Definition bv16 := Z. (* Bvector 16*)
Variant insn :=
  | AInsn : bv16 -> insn
  | CInsn : Dest.t -> Comp.t -> Jump.t -> insn.

Record HackState := {
    Areg : bv16;
    Dreg : bv16;
    PC : bv16;
    mem : bv16 -> bv16
}.

Import Comp.
Definition comp (c : Comp.t) (s : HackState) : bv16 :=
  let d := s.(Dreg) in
  let a := s.(Areg) in
  let m := s.(mem) s.(Areg) in
  match c with
  | ZERO => 0
  | ONE => 1
  | NEGONE => -1
  | D => d
  | A => a
  | M => m

  | NEGD => -d
  | NEGA => -a
  | NEGM => -m
  | INCD => 1 + d
  | INCA => 1 + a 
  | INCM => 1 + m 
  | DECD => d - 1
  | DECA => a -1 
  | DECM => m - 1
  | ADD_DA => d + a
  | ADD_DM => d + m
  | SUB_DA => d + a
  | SUB_DM => d - m
  | SUB_AD => a - d
  | SUB_MD => m - d
  | AND_DA => Z.land d a (* hmmmm. Suspicious *)
  | AND_DM => Z.land d m
  | OR_DA => Z.lor d a
  | OR_DM => Z.lor d m
  (* Negating a two's complement number is simple: Invert all the bits and add one to the result *)
  | NOTD => -d - 1
  | NOTA => -a - 1
  | NOTM => -m - 1
  end .

Import Dest.
Import Jump.
Print Scope Z_scope.
Definition step (i : insn) (s : HackState) : HackState :=
  match i with
  | AInsn addr => {| 
                    Areg := addr;
                    Dreg := s.(Dreg);
                    PC := s.(PC) + 1;
                    mem := s.(mem);
                  |} 
  | CInsn dest c j => 
    let res := comp c s in
    let a' := match dest with
          | A | AM | AD | AMD => res
          | Dest.NULL | M | D | MD => s.(Areg)
    end in
    let d' := match dest with
    | D | MD | AD | AMD => res
    | A | AM | Dest.NULL | M => s.(Dreg)
    end in
    let mem' := match dest with
        | AM | MD | M  | AMD => fun x => if x =? s.(Areg) then res else s.(mem) x
        | A | AD |  Dest.NULL | D => s.(mem)
    end in
    (* Use decide? *)
    let do_jump := match j with
    | NULL => false
    | JGT => res >? 0
    | JEQ => res =? 0
    | JGE => res >=? 0
    | JLT => res <? 0
    | JNE => negb (res =? 0)
    | JLE => res <=? 0
    | JMP => true
    end in
    let pc' := if do_jump then s.(Areg) else 1 + s.(PC) in
    {|
      Areg := a';
      Dreg := d';
      PC := pc';
      mem := mem'
    |}
  end.


Definition init_state : HackState :=
  {|
  Areg := 0;
  Dreg := 0;
  PC := 0;
  mem := fun x => 0
  |}.

Definition prog1 pc :=
  match pc with
  | 0 => AInsn 1 
  | _ => CInsn Dest.D Comp.INCD JMP
  end.
Print nat.

Fixpoint nsteps n (rom : bv16 -> insn) (s : HackState) : HackState :=
  match n with
  | O => s
  | S n' => let insn := rom s.(PC) in
            nsteps n' rom (step insn s) 
  end.

Compute nsteps 5%nat prog1 init_state.


Module StackMachine.
(* https://people.csail.mit.edu/cpitcla/thesis/relational-compilation.html *)
Variant insn :=
  | Push : Z -> insn
  | Pop : insn
  | Add : insn
.
Inductive expr := 
  | Lit : Z -> expr
  | EAdd : expr -> expr -> expr
  .

Fixpoint eval_expr e := match e with
  | Lit x => x
  | EAdd x y => (eval_expr x) + (eval_expr y)
end.

Definition step insn stack :=
  match insn with
  | Push x => Some (x :: stack)
  | Pop => match stack with [] => None | x :: xs => xs end 
  | Add => match stack with 
            | x :: y :: xs => Some (x + y) :: xs
            | _ => None
  end
  end.


End StackMachine.



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