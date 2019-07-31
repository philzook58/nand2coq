{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
module Lib
 where

import qualified Data.Map.Strict as Map
import Linear.V4
import Data.Functor.Compose
import Prelude hiding (Word)
import Data.Bits
import Control.Monad.State.Strict
import Data.Coerce
import Data.Bifunctor
import Data.Word
someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Instruction = At Memory | C Dest CExpr Jump deriving (Eq, Show, Ord)
-- opcode :: Instruction -> Word16



{-
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

-}



type Program = [Instruction] -- [(String, Instruction)] -- Labelled locations?
{-data Memory = Lit Integer | SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}

type Memory = Word
{-| SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}
r0 = 0
r1 = r0 + 1




data Register = D | M | A deriving (Eq, Show, Ord, Enum, Bounded)

data CExpr = Plus X Y | Sub X Y | And X Y | Or X Y | Not X | Reg X | One | NegOne | Zero deriving (Eq, Show, Ord)
-- data COp = Plus | Sub | Or | And | Not | Reg | One | NegOne | Zero -- Keeping operands seperate
-- downside is that we need to supply meaninglss numbers.
-- we could make smart constructors for that
data X = XA | XD | XM deriving (Eq, Show, Ord, Enum, Bounded)
data Y = YA | YD | YM | YOne deriving (Eq, Show, Ord, Enum, Bounded)
type Dest = [Register] -- Ad | Dd | Md | MDd | AMd | ADd | AMDd | Nulld -- [Register] ?

-- data Dest = Dest {dA :: Bool, dM :: Bool, dD :: Bool} -- another option

data Jump =  JGT | JEQ | JGE | JLT | JNE | JLE | JMP | JNull deriving (Eq, Show, Ord, Enum, Bounded)

-- Is this right? I don't think so
cinterp :: CExpr -> Word -> Word -> Word
cinterp (Plus _ _) = (+)
cinterp (Sub _ _) = (-)
cinterp (And _ _) = wand
cinterp (Or _ _) = wor
cinterp (Not _) = \x y -> wnot x
cinterp (Reg _) = error "What is this one?"
cinterp One    = \x y -> wone
cinterp NegOne = \x y -> -1
cinterp Zero = \x y -> wzero


-- we could just cut out the middleman?
getX :: X -> CompState -> Word
getX XA s = regA s
getX XD s = regD s
getX XM s = regM s

-- repetitive. Is it worth reusing more between X and Y?
getY :: Y -> CompState -> Word
getY YA s = regA s
getY YD s = regD s
getY YM s = regM s
getY YOne _ = wone

fillReg :: Register -> Word -> CompState -> CompState
fillReg A w s = s {regA = w} 
fillReg D w s = s {regD = w}  
fillReg M w s = s {regM = w}  

fillDest :: Dest -> Word -> CompState -> CompState
fillDest [] _ s = s 
fillDest (d : ds) w s = fillDest ds w (fillReg d w s)    

-- no
jump :: Jump -> Word -> Bool
jump JGT x = x > 0
jump JEQ x = x == 0
jump JGE x = x >= 0
jump JLT x = x < 0
jump JNE x = x /= 0
jump JLE x = x <= 0
jump JMP _ = True
jump JNull _ = False





(.|) :: CExpr -> Jump -> Instruction
cexpr .| jump = C [] cexpr jump

(.=) :: Dest -> CExpr -> Instruction
dest .= cexpr = C dest cexpr JNull

at :: Memory -> Instruction
at = At

{-
example :: Program
example = [
	at R1,
	Md .= Reg XD
	] 


exampleprog :: State / Writer MutableVector
exampleprog = do
              at 20
			  dm := a .+ d
			  m := 

-}

setPC :: Word -> CompState -> CompState
setPC w s = s {pc = w}

incPC :: CompState -> CompState
incPC s@(CompState{pc=pc}) = s {pc = pc + 1}
-- If I'm using Word, I should just use a vector? Maybe not.  It will be very sparse.
-- and I'm probably not gonna mutate
-- An IntMap would be good though
data CompState = CompState {pc :: Word, regA :: Word, regD :: Word, regM :: Word, ram :: Ram}
{-
interp :: Instruction -> CompState -> CompState
interp (At x) s = s {regA = x}  
interp (C dest expr jmp) s = let res = cinterp expr (getX s) (getY s) in
                               let s' = fillDest dest res s in
							   if (jump jmp res) then incPC s' else setPC res s'
-}
type Ram = Map.Map Word Word
-- default to 0
readRam :: Word -> Ram -> Word
readRam addr ram = Map.findWithDefault addr wzero ram

writeRam :: Word -> Word -> Ram -> Ram
writeRam addr word ram = Map.insert addr word ram 

wzero :: Word
wzero = 0
wone :: Word
wone = 1

wand :: Word -> Word -> Word
wand = (.&.)
wor :: Word -> Word -> Word
wor = (.|.)
wxor :: Word -> Word -> Word
wxor = xor
wnot :: Word -> Word
wnot = complement

-- consider Word16 from Data.Word
-- or better yet Int16 from Data.Int

-- use f traversable instance for addition and such

{-
newtype BitVec f = BitVec (f Bool)  
newtype Word = Word (Compose V4 V4 Bool) deriving (Eq, Ord, Show)
fulladd :: Bool -> Bool -> Bool -> (Bool, Bool) 
fulladd b1 b2 carry = (b1 `xor` b2 `xor` carry, b1 && b2 || b1 && carry || b2 && carry)
bvadd :: (Traversable f, Applicative f) => f Bool -> f Bool -> State Bool (f Bool)
bvadd bv1 bv2 = sequence (state <$> (fulladd <$> bv1 <*> bv2))
-- something very weird is going on with compilation here.

wzero = Word $ pure False
wone = Word $ Compose $ (V4 (V4 True False False False) vzero vzero vzero) where vzero = pure False
-- this is expecting a ziplike applicative instance
wadd :: V4 Bool -> V4 Bool -> (V4 Bool, Bool)
wadd (bv1) (bv2) = runState (bvadd bv1 bv2) False -- irst Word $ 

wadd' :: Word -> Word -> (Word, Bool)
wadd' (Word bv1) (Word bv2) = first Word $ runState (bvadd bv1 bv2) False 

readRam :: Word -> Ram -> Word
readRam addr ram = Map.findWithDefault addr zero ram where zero = Word $ pure False

-}
{-
instance Num Word where

-}







{-
type Word = [Bool]
wzero = replicate 16 False
wone = True : replicate 15 False

wadd c [] _ = (c, [])
wadd c _ [] = (c, [])
wadd c (x : xs) (y : ys) = (c' , (x `xor` y `xor` c) : zs) where
							(c', zs) = (wadd (x && y || x && c || y && c) xs ys)

wand = zipWith (&&)
wor = zipWith (||)
wxor :: Word -> Word -> Word
wxor = zipWith (/=) 
wnot = map not
-}

