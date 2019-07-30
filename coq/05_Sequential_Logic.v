

(*


Less clear what to do here.

Nand2tetris adds a D Flip Flop as a primitive gate.
In our DSL, this would also require loop back or wires but only to The DFF input. The DFF input is allowed to depend on the output.
https://wiki.haskell.org/Circular_programming
Add a DFF primitive that adds a 
DFF :: Circuit () (Bool, Promise Bool)
Cap :: Circuit (Promise Bool, Bool) () -- Eval


data SeqCirc where
  Delay :: SeqCirc -> SeqCirc
  CombCirc :: Circ -> SecCirc



-- A trace
-- traced monoidal category
Delay :: Circuit (a,b) (a,c) -> Circuit b c 

-- The Promise method feels more like a omcpact closed category. But Promise isn't quite a dual
-- we can build an enabled latch using a delay and a multiplexer.

-- (d, (a, s)) =  d' = if s then a else d. dup d' 

Mu :: Circuit ((), a) a -- Monoidal destroyers. did i get the names right? eta? These are not related to the delayed nature. We just didn't need them before
Nu :: Circuit a ((), a)

-- Avoid (). DFFwithEnable : DFF Bool (Bool, Promise Bool)
-- Cap :: (Bool, Promise Bool)


-- not quite right. Anyway. You can make a clock by feeding back neagtive of DFF to itself.
clock = Comp Cap (not' DFF)

counter -- something like Comp Cap add 1 dff16


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

data Promise a -- Promise a  ~  a -> () -- Maybe even want to use this type. Makes Cap (Bool -> (), Bool) () pretty intuitive.
type Next a -- Next might be an alternative name
type X a -- X is modal notation from LTL. Might also be relevant 

State Machines

Build a transition relation function

Kami
http://plv.csail.mit.edu/kami/#
Overkill?




How to interpet a delay circuit
(s,input) -> (s, output)
But what is the type of the state? It is a function of the circuit.
maybe it is slightly more natural to order the state tuple the other way
interesting. It is somewhat like a hidden assoc move.

I have a suspicion that a type system will be quite unhappy with this hiding of state.
SeqCirc s a b


Delay :: SeqCirc s (a,b) (a,c) -> SeqCirc (s,a) b c
CombCirc :: Circ a b -> SeqCirc () a b
StatePar :: SeqCirc s a b -> Seqcirc s' c d -> Seqcirc (s,s') (a,c) (b,d)
Comp :: s  -> s -> s -- same state in all? No. This makes no sense s -> s' -> (s,s'). Each piece brings it's own state with itr
Id :: Seqcirc () a a 
LeftUnit :: Seqcirc ((),s) a b -> Seqcirc s a b -- also may need to insert units. Rearrange?
RightUnit :: 
IsoState :: Seqcirc s -> Seqcirc s' -- We never destroy state. we only can rearrange it? what about (bool, bool) (bool,bool) that swaps?

-- I guess since we don't have any proecinditions on s, we don't really need to include the ()?
-- it is annoying for interpretation.
-- could autofill () into input using typeclasses.
state is like 
bool -> State s bool

-- maybe that is all it needs? Do we need to be able to manipulate () in the state?

seqeval (Delay c) = \((a, a'),x) -> let (b',(b,y)) = (seqeval c) (a',(a,x)) in ((b,b'),y) 
seqeval (CombCirc c) = \(s, a) -> (s, (ceval c) a) -- identity function on the "state"

Hmm but i still need to be able to par dupseq circuits...? Maybe only par.

*)