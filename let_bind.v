    (**
Nand2Coq Chapter 1ish
## In this chapter 

## Formal methods.
    What is the goal of formal methods.
    It is to connect the behavior of a longwinded thing to a succinct and clear thing. Sometimes this feels ass backwards. As we're building the proof, we come to understand the long winded thing was just an unnecessary obfuscation of the short thing. The shortest, clearest spec I can come up with in coq for what the boolean operations mean is to use normal functions built out of the built ins of coq.
     Somewhat this is a bit like proofs themselves. 
     Proofs connect things that are not obviously connected or decompose complicated things into simpler things.
     Every step of a proof is obvious in some sense. If the step is not obvious (due to error or omission), it is no longer a proof in some sense to the consumer of the proof.
     Every step of the proof feels like just a trivial rearrangement pr decopmpsotion. It is by combining many of these small steps that non trivial dsitance is covered. 

    The spec of a boolean functions wrtitten directly in coq is not so hard to see that it is basically the same thing as the function encoded in our hdl.

    https://project-oak.github.io/silveroak/demo/tutorial.html

    *)


(*
Somehow or other we need to get what we mean by the logical connectives into Coq. This will be our specification, the measuring stick by which we measure other constructions.

The most straightforward definition is to write out the truth tables carefully.
*)

Definition nandb x y := match x, y with
  | false, false => true
  | true, false => true
  | false, true => true
  | true, true => false
end.

Definition negb x := match x with
  | false => true
  | true => false
end.

Definition andb x y := match x, y with
  | false, false => false
  | true, false => false
  | false, true => false
  | true, true => true
end.

(* Now we will redefine the booleans operations using only nandb and things previously defined in terms of nandb. *)

Definition negb1 x := nandb x x.
Definition andb1 x y := negb1 (nandb x y).
Definition orb1 x y := nandb (negb1 x) (negb1 y).
Definition norb1 x y := negb1 (orb1 x y).
Definition xorb1 x y := let ny := (negb1 y) in
                        let nx := (negb1 x) in
                        orb (nandb nx x) (nandb y nx).


(* 
We can prove the equivalence of the two defintions. The proof is obvious, we can just exhaustively check the cases.

Note that while the window dressing may look different, this is not appreciatively different the a brute force check you could write in any other language.
The power and flexibilty of Coq comes later.


For example in python

bools = [True,False]

def negb(x):
    return nandb(x,x)

def negb_proof():
    return all( [     ] )

*)

Theorem negb_equiv : forall x : bool, negb1 x = negb x. 
    intros; destruct x; reflexivity.

(* Tactic my_tac := intros x y; destruct x y; reflexivity. *)

(*
Exercise: try the other cases.
*)

(*
A slight shift in perspective is to build a data type to represent our circuits. Then we can interpret this data type and see again that we are talking about the same thing. Bored yet?
*)


Inductive TwoPort :=
    | Xin 
    | Yin
    | Nandb : TwoPort -> TwoPort -> TwoPort.

Fixpoint interp (circ : TwoPort) (x y : bool) : bool :=
    match circ with
    | Xin => x
    | Yin => y
    | Nandb a b => nandb (interp a x y) (interp b x y)
    end.


Definition Negb (x : TwoPort) := Nandb x x.

(*
There is something unsatisfying about our data type in that we cannot properly express sharing. In other words we can't reuse a computation without rebuilding it's entire circuit

For this a common programming language construct is the let binding.



*)


(*
The first challenge is to build a data type that can describe logical circuits
This is the same challenge as any language. A thing you might reach for is a graph library


Cody's example
https://github.com/codyroux/tinymatch/blob/main/tinylang.v

*)
Search bool.

Compute andb true false.
Compute orb true false.
(* Definition nandb x y := negb (andb x y). *)

Inductive ShareCirc var :=
   (* | Input : var -> ShareCirc var *)
   (* | Xin | Yin    *)
   | Var : var -> ShareCirc var
   | Let : var -> ShareCirc var -> ShareCirc var
   | Nandb : ShareCirc var -> ShareCirc var -> ShareCirc var
   .

   (*
   
   *)

(*

Inductive Key := X | Y.

Class Lookup key record := {
    lookup : key -> record -> bool
}

And let could persay intrdouce a new record.

Inductive ShareCirc key rec :=
   Var : key -> ShareCirc
   Let : sum unit key -> ShareCirc key (bool * rec) -> 
This is the low key dependenlty typed version of Vec n
where I use typeclases and * to build it

Record abc_env := {
 a : bool;
 b : bool;
 c : bool;
}.

Record

(* I wonder if if will accept this. *)
Record Sixteen {
    0 : bool;
    1 : bool;
    2 : bool;
    3 : bool;
    ...
}


    Record Sixteen := {
        b0 : bool;

        b1 : bool;
        b2 : bool;
        b3 : bool
        
    }.

Definition vec n a :=
     | => unit 
     | =>  a * (vec n-1 a) 



This unfortunately allows us to express ill-formed circuits. We cannot use a variable that is never defined, or even a vairable before it is defined. In fact to allow the latter would introduce loops into our circuit, something we wish to explicitly disallow for combinatorial circuits.

So now in the case our circuit is ill-formed, the intepreter has to fail.




*)

Definition StringMap a := list (string * a).



Definition interp ShareCirc : Maybe (StringMap bool => bool)



(*

A note of obfuscation. You may choose the express conditions like
forall l, In "x" l -> In "y" l -> yada yada
This may be a pleasing language to you. However, you should decide if it is worth adding this as it will always keep introducing proof burdens. Which you will then automate away because it is equiavalent to the other form.

It is debatable if there is much benefit to rolling and unrolling facts other than to spin your wheels.


Rather than an intepretation taking a list of bools, where the producer gets to pick the number of bools which may be incorrect, it is intrinsically well typed to have the consumer choose the number of bools to accept

These are what are known as coroutines. They result from rearranging the ordering of Cons and lambdas.




Inductive ManyBool a :=
  | More : (bool -> ManyBool a) -> MaynBool a
  | Done : a -> ManyBool a
  .

ManyKeyedBool may request particular named keys.

Inductive ManyKeyedBool a :=
  | More : string -> (bool -> ManyBool a) -> MaynBool a
  | Done : a -> ManyBool a
   
We can pass on up the request to the environment if we do not know the answer.
This is still partiality like using Option, but of a different sort. In one sense it is an extension like an error monoad. Now we know the reason why we failed, "a" missing. But there is more, we also how to continue the computation if the missing "a" is supplied. So one might consider this a kind of error recovery monad.


Another approach is to parametrize over the environment so that there is an explicit way to read from a well typed env. Does that save us from anything?
   env

Reader env a

Read : (env -> Circlang) -> CircLang
This is again a kind of hoas.




*)

(*
QuickChick

*)


(*

Similar to how we wanted to express share wires in a ciruit in order to avoid inlining identical definitions at every use, we want to share circuit definitions as modules so that we don't have to inline the definition of the module at every turn.

Definition ModuleImplements m f := interp m ["x" ]= Some (f x y z)

The brute force checking could be done in any tool, but foolproof modular reasoning requires something a little extra that Coq provides


forall m1 m2 m3, ModuleImplements "Andb" andb,
                 ModuleImplements "Negb" negb,
                 ..

Definition Stdlib := {
     nandb_mod :  ModuleImplements "Nandb" nandb 

}


*)

Require Import String.
Open Scope string_scope.



Definition bind {a b : Type} (x : option b) f : option a := match x with
   | Some x => f x
   | None => None
end.

(* https://github.com/coq-community/coq-ext-lib/blob/master/theories/Structures/Monad.v *)
Check @bind.
Notation "x <- c1 ;; c2" := (@bind _ _ c1 (fun x => c2))
(at level 61, c1 at next level, right associativity).

Definition bind_test : option nat :=
    x <- Some 4;;
    _ <- Some 5;;
    y <- Some 5;;
    Some (x + y)
    .
Compute bind_test.

(*
However, it would be more familiar and 
probably easier to write if we just made the arguments positional.

It is indeed a property of these circuits that the list structure will be known ahead of time can be partially evaluated away.

I can either use Option, or I can use relation technology by defining an inductive.


Ah. Nand2Tetris supports arrays of wires.
Makes sense.
*)



Require Import List.
Import ListNotations.
Print Scope list_scope.
Fixpoint lookup {a : Type} (s : string) (l : list (string * a)) : option a := match l with
  | (s', v) :: l => if String.eqb s' s then Some v else lookup s l  
  | [] => None
end.

Fixpoint flatmap {a b : Type } (f : a -> option b) (xs : list a) : option (list b) := match xs with
 | x :: xs => x' <- f x;;
              xs <- flatmap f xs;;
              Some (x' :: xs)
 | [] => Some []
end.

Module circ1.

    Inductive circuit :=
    | Wire : string -> circuit
    | Nand : circuit -> circuit -> circuit.  

    Fixpoint interp c inputs :=
        match c with
        | Wire s => lookup s inputs
        | Nand c1 c2 => 
            bind (interp c1 inputs) (fun b1 =>
            bind (interp c2 inputs) ( fun b2 =>
            Some (nandb b1 b2))) 
        end.
    Compute "aaa".

    Eval compute in interp (Wire "a") [ ("a", true) ].

    Eval compute in interp (Nand (Wire "a") (Wire "a")) [ ("a", true) ; ("b", false) ].
End circ1.


Module circ2.

    Inductive bexpr :=
    | Wire : string -> bexpr
    | Nand : bexpr -> bexpr -> bexpr. 
    Inductive circuit :=
    | Output : string -> bexpr -> circuit -> circuit
    | Let : string -> bexpr -> circuit -> circuit
    | Done : circuit.


    Fixpoint interp_bexp c inputs :=
        match c with
        | Wire s => lookup s inputs
        | Nand c1 c2 => 
            bind (interp_bexp c1 inputs) (fun b1 =>
            bind (interp_bexp c2 inputs) ( fun b2 =>
            Some (nandb b1 b2))) 
        end.
    Fixpoint interp c inputs :=
        match c with
        | Let s b c =>  
            bind (interp_bexp b inputs) (fun b =>
            interp c (  (s, b ) :: inputs))
        | Output s b c => bind (interp_bexp b inputs) (fun b =>
                          bind (interp c inputs) (fun res =>
                          Some ((s, b) :: res)  ))
        | Done => Some []
        end.
    
        
    Definition neg (x : bexpr) : bexpr := 
        Nand x x.
    Definition negc (s : string) : circuit :=
        Output "a" (Wire s) Done.
    Definition negc2 : circuit :=
        let a := Wire "a" in 
        Output "b" (Nand a a) Done.

    Theorem neggo (a : bool) : interp negc2 [("a", a)] = Some [("b" , negb a)].
    compute.
    case a; auto. Qed.


    End circ2.



    Module circ3.

    (**
    Really this is a first order functional language with named parameters and output parameters without recursion more than it is an expression language.
    Although brute force proof will scale to these initial examples. 

    
    *)
    Record module_call := {
       name : string;
       ins : list (string * string);
       outs : list (string * string)
    }.
    Record circuit :=
        {
         in_wires : list string;
         out_wires : list string;
         call_modules : list module_call
        }.

    (*
    Make a nice notation. While in general as a personal preference I abhor specialized notations and mathemtical symbols in computerized systems, in this case it does raise the clarity of the definitions
    
    *)
    Check flatmap.
    Definition translate_keys {a : Type}
    (kv : list (string * a) )
    (trans : list (string * string))  
    : option (list (string * a)):=
    flatmap (fun kk =>
    let (old_key, new_key) := kk : string * string in
    v <- lookup old_key kv;;
    Some (new_key, v) ) trans.

    
    Definition swap {a b : Type} xy : b * a := 
        match xy with (x,y) => (y,x) end.
    Fixpoint interp 
    (c : list module_call)
     (modules : list (string * (list (string * bool) -> option (list (string * bool) )))) 
     (inputs : list (string * bool) ): option (list (string * bool)) :=
        match c with
        | c :: cs =>
           let in_map := List.map swap c.(ins) in
           mod_inputs <- translate_keys inputs in_map;;
           m <- lookup c.(name) modules;;
           mod_outs <- m mod_inputs;;
           
           new_wires <- translate_keys mod_outs c.(outs) ;;
           interp cs modules (new_wires ++ inputs)
        | [] => Some inputs
        end.
    Definition nand_context := [ ("Nand", 
        fun inputs =>
        a <- lookup "a" inputs;;
        b <- lookup "b" inputs;;
        Some [("c",  nandb a b)])
    ].
    Definition mod_nand_ex :=  {| name := "Nand";
    ins := [ ("a", "x") ; ("b", "y")  ] ;
    outs := [ ("c", "z")  ]  |}.

    Eval compute in interp 
      [mod_nand_ex]
      nand_context [("x", false) ; ("y", true) ].

    Theorem mod_nand_theorem (a b : bool) : 
    interp 
    [mod_nand_ex]
    nand_context [("x", a) ; ("y", b) ] = 
    Some [("z", nandb a b) ; ("x", a) ; ("y", b) ] .
    auto. Qed.

        (* A sequence of circuit definitions *)
    Definition module_seq := list (string * circuit). 
    Definition mod_interp := list (string * bool) -> option (list (string * bool)).

    (* Fixpoint interp_mod_seq (ms : module_seq) 
      (mod_context : list (string * mod_interp))  :=
        match ms with
        | m :: ms => 
              let mod_interp := 
                 fun in_map =>
                  <_ 
                 interp m.(module_calls) mod_context in 
              new_mod :: (interp_mod_seq ms (new_mod :: mod_context))
        | [] => mod_context
        end.   *)
End circ3.

Module circ4.
Record func_call := {
    name : string;
    args : list string;
 }.
   Record func_def :=
     {
      params : list string;
      body   : list (list string * func_call);   
      return_vars : list string;
     }.
     (* The body is a bunch of let statements calling functions in order. 
     *)

     Definition func_def_seq := list (string * func_def).
     (*
     We could use a dependent type instad of lists of arguments.
     Functions are not allowed to have varargs.
     *)

     End circ4.






Module flipflop.

Definition State s a := s -> a * s.

Definition flip_flop (x : bool) : State bool bool :=
    fun _ => (x, x).

Definition pure {s a : Type} (x : a) : State s a :=
    fun s => (x, s).

Definition fmap {s a b : Type} (f : a -> b) (x : State s a) : State s b := fun s => let (y, s2) := x s in (f y, s2).

Definition clock : State bool bool := 
    fun s => ( s, negb s).
    
    





(**
We may have to come back to the drawing board for flip flop

One natural approach is to simulate a single step

We allow recursion, but only if mediated by DFF

We could model this as perhaps a function with mutable state variable.
Assign only once in function body.
We then have let statements and assignment statements := 

Then outer loop = clocking
while(true){
    call(module);
}


Record module_def {
    regs : list string; (* or d flip flop *)
    inputs : list string:
    outputs : list string;
            : Module mod_val
}


Definition step ( state : ) (inputs ) : outputs * state


Definition step_rel in out :=  step in = out.

Inductive step : state -> state -> Prop :=
  


*)   



End flipflop.


    (* Right. It doesn't even matter what the definition of nandb is. There is enough concrete info in those lists that it progresses completely to the same side. 

    *)
 
 

    (* WellFormed ->  interp x = Some y *)

    Fixpoint interp_bexp c inputs :=
        match c with
        | Wire s => lookup s inputs
        | Nand c1 c2 => 
            bind (interp_bexp c1 inputs) (fun b1 =>
            bind (interp_bexp c2 inputs) ( fun b2 =>
            Some (nandb b1 b2))) 
        end.
    Fixpoint interp c inputs :=
        match c with
        | Let s b c =>  
            bind (interp_bexp b inputs) (fun b =>
            interp c (  (s, b ) :: inputs))
        | Output s b c => bind (interp_bexp b inputs) (fun b =>
                          bind (interp c inputs) (fun res =>
                          Some ((s, b) :: res)  ))
        | Done => Some []
        end.
    
        
    Definition neg (x : bexpr) : bexpr := 
        Nand x x.
    Definition negc (s : string) : circuit :=
        Output "a" (Wire s) Done.
    Definition negc2 : circuit :=
        let a := Wire "a" in 
        Output "b" (Nand a a) Done.

    Theorem neggo (a : bool) : interp negc2 [("a", a)] = Some [("b" , negb a)].
    compute.
    case a; auto. Qed.


    End circ2.



    Compute "aaa".

    Eval compute in interp (Wire "a") [ ("a", true) ].

    Eval compute in interp (Nand (Wire "a") (Wire "a")) [ ("a", true) ; ("b", false) ].
End circ1.

 

Inductive circuit_val :=
  | True : circ
  | False : cval
  | Var : string -> cval

Inductive circuit :=
  | True : circuit
  | False : circuit
  | Nand : circuit -> circuit -> circuit
  | Tup : circuit -> circuit -> circuit
  | LetWire : string -> circuit -> circuit
  | Wire : string -> circuit
  | 

Inductive hdl := Module  
   
Record module := {
    name : string
    ins : list (string * string);
    outs : list (string * string) 
}

Record chip := {
    name : string
    ins : list string;
    outs : list string; 
    modules : list module
}

Definition 

Definition well_formed (chip : chip) :   := 

Definition unique_outs chip := 
    forall x in m.outs, exists unique v, (v,x) in chip.modules.outs 

Definition dag chip := (* ordering exists *)



Definition well_formed (chip : chip) := 
    unique_outs chip /\ no_dangling_in /\ dag chip.


Definition find_out (m : list module) (x : string) := match m with 
  | m :: ms => if find_key m.outs x then
                | None => find_out ms x
                | Some v => v
  | [] => None 
end.



Definition interp (chip : chip) (ins : list (string * bool)) : 
   := 
    chip.outs


(* cexpr
One model of a combinatorial circuit is as boolean expressions with let bindings
*)
Inductive circuit := 
  | Lit : cval -> circuit
  | Let : (cval -> circuit) -> circuit
  | Nand : circuit -> circuit -> circuit
  | 

(*
Check well formedness of circuit?
It is decidable.

*)