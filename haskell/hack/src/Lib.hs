module Lib
 where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Instruction = At Memory | C Dest CExpr Jump


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


(.|) :: CExpr -> Jump -> Instruction
cexpr .| jump = C Nulld cexpr jump

(.=) :: Dest -> CExpr -> Instruction
dest .= cexpr = C dest cexpr JNull

at :: Memory -> Instruction
at = At


example :: Program
example = [
	at R1,
	Md .= Reg XD
	]







