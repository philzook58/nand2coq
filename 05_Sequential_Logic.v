

(*


Less clear what to do here.

Nand2tetris adds a D Flip Flop as a primitive gate.
In our DSL, this would also require loop back or wires but only to The DFF input. The DFF input is allowed to depend on the output.
https://wiki.haskell.org/Circular_programming
Add a DFF primitive that adds a 
DFF :: Circuit () (Bool, Promise Bool)
Cap :: Circuit (Promise Bool, Bool) () -- Eval
Mu :: Circuit ((), a) a -- Monoidal destroyers. did i get the names right? eta?
Nu :: Circuit a ((), a)

The data flow arrows have directionality. data flows out of input bools into output bools
data flows into input Pormise bools and out of output Promise bools


Cup :: Circuit () (Promise Bool, Bool) -- Get a bool from up top. We Don't want this because we only want DFF, not general loop back
-- These might be derived from cup?
Flip :: Circuit a b -> Circuit () (Promise a, b)
-- this is derived from cap and eval. Use if we want the DFF to actually have the input be an input and not feedback
Flup :: Circuit () (Promise a, b) -> Circuit a b

flup Par = 
flup Dup = 
flup ...

data Promise a -- Promise a  ~  a -> ()


State Machines

Build a transition relation function

Kami
http://plv.csail.mit.edu/kami/#
Overkill?







*)