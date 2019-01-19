module Main where

import Lib
import Text.Parsec
import Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

main :: IO ()
main = parseTest (oneOf "aeiou") "a" -- someFunc

lexer :: Tok.TokenParser ()
lexer = makeTokenParser style
  where
    ops = ["@","=",";","-","+",""]
    names = registers ++ ["A","M","D"] ++ IO'
    IO' = ["SCREEN", "KBD"]

    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.reservedOpNames = ops
             , Tok.caseSensitive = True
             , Tok.reservedNames = names
             }

registers = map ('R':) $ map show [0 .. 15]
jumps = ["JEQ", "JGT", "JGE", "JLT", "JLE", "JMP", "JNE", ]
--manyOf line  eof
{-
regD = char 'D'
regM = char 'M'
regA = char 'A'

reg = oneOf "DMA"

at = char "@"
-}

numericLit ::Parsec String () Memory
numericLit = Lit . read <$> many1 digit {-do 
	           x <- many1 digit
	           return $ Lit (read x) -}

xreg :: Parsec String () Register
xreg = read . return <$> oneOf "DMA"{-do 
		r <- oneOf "DMA"
		return $ case r of
					'D' -> XD
					'M' -> XM
					'A' -> XA -}
{-					
yreg :: Parsec String () Y
yreg = 	do
		r <- oneOf "DMA1"
		return $ case r of
					'D' -> YD
					'M' -> YM
					'A' -> YA
					'1' -> YOne
comment :: Parsec String () ()
comment = spaces >> string "//" >> noneOf "\n" >> endOfLine >> return ()

command = ainstr <|> cinstr

cinstr = anyChar
var = Var <$> (:) <$> letter <*> many alphaNum

ainstr =  char "@" >> (numericLit <|> Var 
-}

{-
parsefile = many $ do
			many comment 
			cmd <- readCommand
			endOfLine
			return cmd
-}
{-
do at
   <- (letter >>= (many alphaNum)) <|> ()
-}

