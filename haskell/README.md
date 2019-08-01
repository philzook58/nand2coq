# Haskell Nand 2 Tetris

Haskell is a much more familiar language to me than either Verilog or Coq. It is highly suitable for computer sciency tasks. Also I love it in general.

One goal in specification is to have representations of your system at different levels of abstraction and connect them all together formally (they do the same thing in the appropriate sense).

Files:
  - src/Circuit.hs -- a low level circuit HDL. Gate level stuff
  - src/CPU.hs -- a functional model of the CPU
  - src/Assembly.hs - a representation of the Hack assembly language and an intepreter


It is clear and useful to write interpreters for the various levels of the Nand 2 tetris project in Haskell. We can connect these different specifications to one another.

We have a couple of different useful options:

A Lightweight formal method for giving confidence in properties is the quickcheck library

http://hackage.haskell.org/package/QuickCheck

A powerful connection to SMT solvers for the purposes of formal methods can be found in the SBV library.

http://hackage.haskell.org/package/sbv

It may be a good idea to proceed with Liquid Haskell for doing the proving process in Haskell itself.

https://ucsd-progsys.github.io/liquidhaskell-blog/

Another alternative is to use the hs-to-coq tool to extract coq for the purposes of proving properties.

https://github.com/antalsz/hs-to-coq



---

### Haskell Tooling

Using stack
`stack build` compile the files
`stack ghci` interactive repl

Making Haskell Tutorials
https://yannesposito.com/Scratch/en/blog/Haskell-Tutorials--a-tutorial/

Haddock for generating docs.
`stack haddock`
 Put bars in comments to make them appear in documentation
-- |

{- | 
-}

You can also place code examples

>>>

Maybe we should just use literate haskell files?
Or perhaps something pandoc able?
