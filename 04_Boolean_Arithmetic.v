
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