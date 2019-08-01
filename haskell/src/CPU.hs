module CPU where

import Data.Word
{-

CPU


https://github.com/wyager/CPU
https://yager.io/CPU/CPU1.html



Functional Specification of computer pieces.
Put

TODO:
Get all the high level pieces we need.
Copy the appropraute excerpts from the book
implement them.

-}


{-| $ Excerpt Figure 5.2


Chip Name: CPU // Central Processing Unit
Inputs:

    inM[16],          // M value input  (M = contents of RAM[A])
    instruction[16],  // Instruction for execution
    reset
Outputs:
    outM[16],
    writeM,
    addressM[15],
    pc[15]
// Signals whether to restart the current
// program (reset=1) or continue executing
// the current program (reset=0)
// M value output
// Write to M?
// Address of M in data memory
// Address of next instruction
Function: Executes the instruction according to the Hack machine language specification. The D and A in the language specification refer to CPU-resident registers, while M refers to the memory location addressed by A (inM holds the value of this location).
           If the instruction needs to write a value to M, the value is
           placed in outM, the address is placed in addressM, and the writeM
           bit is asserted. (When writeM=0, any value may appear in outM.)
           If reset=1, then the CPU jumps to address 0 (i.e., sets pc=0 in
           the next time unit) rather than to the address resulting from
           executing the current instruction.
-}
data CPUIn = CPUIn {memIn :: Word16,  instruction :: Word16, reset :: Bool}
data CPUOut = CPUOut {memOut :: Word16,  pc :: Word16, writeM :: Bool, addressM :: Word16} 

-- | implement a function specification of the cpu
cpu :: CPUIn -> CPUOut
cpu CPUIn{memIn=memIn, instruction=instruction, reset=reset} = error "To be implemented"


alu :: () -> ()
alu _ = error "to be implemented"


-- | Stateful components



