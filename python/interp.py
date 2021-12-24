from dataclasses import dataclass
from enum import Enum       
import typing
from typing import List, Union

Comp = Enum('Comp', '''ZERO ONE NEGONE D A M NOTD NOTA NOTM NEGD NEGA NEGM INCD INCA INCM DECD DECA DECM 
                         ADD_DA ADD_DM SUB_DA SUB_DM SUB_AD SUB_MD AND_DA AND_DM OR_DA OR_DM''') 
Dest = Enum('Dest', 'NULL M D MD A AM AD AMD') 
Jump = Enum('Jump', 'NULL JGT JEQ JGE JLT JNE JLE JMP') 

@dataclass
class Ainstr:
    lit: int

@dataclass
class Cinstr:
    dest: Dest
    comp: Comp
    jump: Jump

@dataclass
class HackState:
    rA: int
    rD: int
    pc: int
    mem: List[int]

def execute(instr : Cinstr, s : HackState):
    A = s.rA
    D = s.rD
    pc = s.pc
    M = s.mem[A]

    if instr.comp == Comp.ZERO:
        comp = 0
    elif instr.comp == Comp.ONE:
        comp = 1
    elif instr.comp == Comp.NEGONE:
        comp = -1
    elif instr.comp == Comp.D:
        comp = D
    elif instr.comp == Comp.A:
        comp = A
    elif instr.comp == Comp.M:
        comp = M
    elif instr.comp == Comp.NOTD:
        comp = ~D
    elif instr.comp == Comp.NOTA:
        comp = ~A
    elif instr.comp == Comp.NOTM:
        comp = ~M
    elif instr.comp == Comp.NEGD:
        comp = -D
    elif instr.comp == Comp.NEGA:
        comp = -A
    elif instr.comp == Comp.NEGM:
        comp = -M
    elif instr.comp == Comp.INCD:
        comp = D + 1
    elif instr.comp == Comp.INCA:
        comp = A + 1
    elif instr.comp == Comp.INCM:
        comp = M + 1
    elif instr.comp == Comp.DECD:
        comp = D - 1
    elif instr.comp == Comp.DECA:
        comp = A - 1
    elif instr.comp == Comp.DECM:
        comp = M - 1
    elif instr.comp == Comp.ADD_DA:
        comp = D + A
    elif instr.comp == Comp.ADD_DM:
        comp = D + M
    elif instr.comp == Comp.SUB_DA:
        comp = D - A
    elif instr.comp == Comp.SUB_DM:
        comp = D - M
    elif instr.comp == Comp.SUB_AD:
        comp = A - D
    elif instr.comp == Comp.SUB_MD:
        comp = M - D
    elif instr.comp == Comp.AND_DA:
        comp = D & A
    elif instr.comp == Comp.AND_DM:
        comp = D & M
    elif instr.comp == Comp.OR_DA:
        comp = D | S
    elif instr.comp == Comp.OR_DM:
        comp = D | M
    else:
        assert False

    if instr.dest == Dest.NULL:
        pass
    elif instr.dest == Dest.M:
        M = comp
    elif instr.dest == Dest.D:
        D = comp
    elif instr.dest == Dest.MD:
        M = comp
        D = comp
    elif instr.dest == Dest.A:
        A = comp
    elif instr.dest == Dest.AM:
        A = comp
        M = comp
    elif instr.dest == Dest.AD:
        A = comp
        D = comp
    elif instr.dest == Dest.AMD:
        A = comp
        M = comp
        D = comp
    else:
        assert False
        
    if instr.jump == Jump.NULL:
        jump = False
    elif instr.jump == Jump.JGT:
        jump = comp > 0
    elif instr.jump == Jump.JEQ:
        jump = comp == 0
    elif instr.jump == Jump.JGE:
        jump = comp >= 0
    elif instr.jump == Jump.JLT:
        jump = comp < 0
    elif instr.jump == Jump.JNE:
        jump = comp != 0
    elif instr.jump == Jump.JLE:
        jump = comp <= 0
    elif instr.jump == Jump.JMP:
        jump = True
    else:
        assert False
    
    # we may not want to mutate here.
    # return comp, jump, dest instead?
    if jump:
        s.pc = s.rA
    else:
        s.pc = pc + 1
    
    s.mem[s.rA] = M #??? What is the order here?
    s.rA = A
    s.rD = D

    # return A, D, M, jump ?
    return s

Instr = Union[Ainstr,Cinstr]

def fetch(rom : List[Instr], s : HackState) -> Instr:
    return rom[s.pc]

def run(rom):
    # hmm. We should make 16 bit vectors.
    state = HackState(0,0,0,[0]*2**13)
    while state.pc != -1:
        instr = fetch(rom, state)
        state = execute(instr, state)
    return state