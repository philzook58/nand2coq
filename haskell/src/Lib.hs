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
someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Instruction = At Memory | C Dest CExpr Jump
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

type Program = [Instruction] -- [(String, Instruction)] -- Labelled locations?
data Memory = Lit Integer | SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show

data Register = D | M | A deriving (Read, Show)

data CExpr = Plus X Y | Sub X Y | AND X Y | OR X Y | Not X | Reg X | One | NegOne | Zero

data X = XA | XD | XM
data Y = YA | YD | YM | YOne
data Dest = Ad | Dd | Md | MDd | AMd | ADd | AMDd | Nulld
data Jump =  JGT | JEQ | JGE | JLT | JNE | JLE | JMP | JNull

-}



type Program = [Instruction] -- [(String, Instruction)] -- Labelled locations?
{-data Memory = Lit Integer | SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}

data Memory = WordLit Word 
{-| SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}
r0 = 0
r1 = r0 + 1




data Register = D | M | A deriving (Read, Show)

data CExpr = Plus X Y | Sub X Y | AND X Y | OR X Y | Not X | Reg X | One | NegOne | Zero

data X = XA | XD | XM
data Y = YA | YD | YM | YOne
data Dest = Ad | Dd | Md | MDd | AMd | ADd | AMDd | Nulld
data Jump =  JGT | JEQ | JGE | JLT | JNE | JLE | JMP | JNull


(.|) :: CExpr -> Jump -> Instruction
cexpr .| jump = C Nulld cexpr jump

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
-}

data CompState = CompState {regA :: Word, regD :: Word, regM :: Word, ram :: Map.Map Word Word}

newtype Word = Word (Compose V4 V4 Bool) deriving (Eq, Ord, Show)
type Ram = Map.Map Word Word
-- use f traversable instance for addition and such
newtype BitVec f = BitVec (f Bool)  


fulladd :: Bool -> Bool -> Bool -> (Bool, Bool) 
fulladd b1 b2 carry = (b1 `xor` b2 `xor` carry, b1 && b2 || b1 && carry || b2 && carry)
bvadd :: (Traversable f, Applicative f) => f Bool -> f Bool -> State Bool (f Bool)
bvadd bv1 bv2 = sequence (state <$> (fulladd <$> bv1 <*> bv2))


wzero = Word $ pure False
wone = Word $ Compose $ (V4 (V4 True False False False) vzero vzero vzero) where vzero = pure False
-- this is expecting a ziplike applicative instance
wadd :: V4 Bool -> V4 Bool -> (V4 Bool, Bool)
wadd (bv1) (bv2) = runState (bvadd bv1 bv2) False -- irst Word $ 

wadd' :: Word -> Word -> (Word, Bool)
wadd' (Word bv1) (Word bv2) = first Word $ runState (bvadd bv1 bv2) False 
{-
instance Num Word where

-}


-- default to 0
readRam :: Word -> Ram -> Word
readRam addr ram = Map.findWithDefault addr zero ram where zero = Word $ pure False

writeRam :: Word -> Word -> Ram ->  Ram
writeRam addr word ram = Map.insert addr word ram 


-- interp :: Instruction -> CompState -> CompState
-- interp (At (Lit x)) st = st {regA = x}  




