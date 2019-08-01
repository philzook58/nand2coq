{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, BinaryLiterals #-}
module Assembly where

import qualified Data.Map.Strict as Map
import Linear.V4
import Data.Functor.Compose
import Prelude hiding (Word)
import Data.Bits
import Control.Monad.State.Strict
import Data.Coerce
import Data.Bifunctor
import Data.Word -- should be Data.Int

-- http://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Bits.html#t:Bits

-- | https://www.nand2tetris.org/project04 

-- | a program consists of a sequence of instructions
type Program = [Instruction] -- [(String, Instruction)] -- Labelled locations?

{-| 
Instructions are categorized into A insturctions and C instructions
A-instructions set the A register to a constant. This constant can be used for different purposes. It loads a literal, sets up the M register to point to that location, and sets where a jump instruction will jump to.
C-instructions compute a couple different functions on the contents of the registers A, M, and D and store the result in between none and all of AMD. There may also be a jump occuring to a different point of the program based on the result of the computation.
-}
data Instruction = At Memory | C Dest CExpr Jump deriving (Eq, Show, Ord)


-- | opcode translate an instruction into it's 16 bit opcode
-- opcode :: Instruction -> Word16
-- opcode (At x) = (16 << 1) .|. x
-- opcode (C dest expr jmp) = (? << ) .|.  ((operation_code expr) << 3 ) .|.  ( jump_code jmp)







-- Do we want to use a structured form of memory or raw Word?
-- We could also have sugared and desugared assembly
-- sugared assembly may have variable names, named program locations, etc
{-data Memory = Lit Integer | SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}

type Memory = Word
{-| SCREEN | KBD | Var String |
	R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | 
	R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving Show -}
-- r0 :: Memory
-- r0 = 0
-- r1 = 1




data Register = D | M | A deriving (Eq, Show, Ord, Enum, Bounded)

data CExpr = Plus X Y | Sub X Y | And X Y | Or X Y | Not X | Reg X | One | NegOne | Zero deriving (Eq, Show, Ord)
-- data COp = Plus | Sub | Or | And | Not | Reg | One | NegOne | Zero -- Keeping operands seperate
-- downside is that we need to supply meaninglss numbers.
-- we could make smart constructors for that

-- | The X operand can be any of the registers
data X = XA | XD | XM deriving (Eq, Show, Ord, Enum, Bounded)
-- newtype X = X Register
-- | The Y operand can be any of the registers or the constant 1.
data Y = YA | YD | YM | YOne deriving (Eq, Show, Ord, Enum, Bounded)
-- data Y = YOne | YReg Register

-- type Dest = [Register] -- Ad | Dd | Md | MDd | AMd | ADd | AMDd | Nulld -- [Register] ?

-- | The result of the C-insturction computation may be stored in any subset of the AMD registers.
data Dest = Dest {dA :: Bool, dM :: Bool, dD :: Bool} deriving (Eq, Show, Ord) -- another option

-- | Jump instructions compare the result with zero. No jump or unconditional jump are also possible
data Jump =  JNull | JGT | JEQ | JGE | JLT | JNE | JLE | JMP  deriving (Eq, Show, Ord, Enum, Bounded)


-- could also use enum instance, but it just isn't worth it. Too fancy.
-- | Translating the jump data type to it's corresponding bitcode
-- | figure 4.5 page 69
jump_bitcode :: Jump -> Word
jump_bitcode JNull =  0b000
jump_bitcode JGT = 0b001
jump_bitcode JEQ = 0b010 
jump_bitcode JGE = 0b011
jump_bitcode JLT = 0b100
jump_bitcode JNE = 0b101 
jump_bitcode JLE = 0b110
jump_bitcode JMP = 0b111


-- Is this right? I don't think so
-- | Converting the operand into it's corresponding Haskell function for the purposes of simulation
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
-- | Helper functions to get the X operands out of the state.
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
{-
fillDest :: Dest -> Word -> CompState -> CompState
fillDest [] _ s = s 
fillDest (d : ds) w s = fillDest ds w (fillReg d w s)    
-}
-- no

-- | Translating the jump conditions to their corresponding Haskell function
jump :: Jump -> Word -> Bool
jump JGT x = x > 0
jump JEQ x = x == 0
jump JGE x = x >= 0
jump JLT x = x < 0
jump JNE x = x /= 0
jump JLE x = x <= 0
jump JMP _ = True
jump JNull _ = False







{-
If we choose to write programs inside of haskell, can we use do notation to make everything look nice?
Yes.

(.|) :: CExpr -> Jump -> Instruction
cexpr .| jump = C [] cexpr jump

(:=) :: Dest -> CExpr -> Instruction
dest := cexpr = C dest cexpr JNull

at :: Memory -> Instruction
at = At

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

