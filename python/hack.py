from dataclasses import dataclass
from enum import Enum       
from typing import List, Union

Comp = Enum('Comp', '''ZERO ONE NEGONE D A M NOTD NOTA NOTM NEGD NEGA NEGM INCD INCA INCM DECD DECA DECM 
                         ADD_DA ADD_DM SUB_DA SUB_DM SUB_AD SUB_MD AND_DA AND_DM OR_DA OR_DM''') 
Dest = Enum('Dest', 'NULL M D MD A AM AD AMD') 
Jump = Enum('Jump', 'NULL JGT JEQ JGE JLT JNE JLE JMP') 

A = Dest.A
D = Dest.D
M = Dest.M

R0 = 0
R1 = 1
R2 = 2
R3 = 3
R4 = 4

@dataclass
class AInsn:
    lit: int

@dataclass
class CInsn:
    dest: Dest
    comp: Comp
    jump: Jump

@dataclass
class HackState:
    rA: int
    rD: int
    pc: int
    mem: List[int]

Insn = Union[AInsn,CInsn]

#Tables from Chpater 5 slide 33
comp_asm = {
    "0" : Comp.ZERO,
    "1" : Comp.ONE,
    "-1" : Comp.NEGONE,
    "D" : Comp.D,
    "A" : Comp.A,
    "M" : Comp.M,
    "~A" : Comp.NOTA,
    "~M" : Comp.NOTM,
    "-D" : Comp.NEGD,
    "-A" : Comp.NEGA,
    "-M" : Comp.NEGM,
    "D+1" : Comp.INCD,
    "A+1" : Comp.INCA,
    "M+1" : Comp.INCM,
    "D-1" : Comp.DECD,
    "A-1" : Comp.DECA,
    "M-1" : Comp.DECM,
    "D+A" : Comp.ADD_DA,
    "D+M" : Comp.ADD_DM,
    "D-A" : Comp.SUB_DA,
    "D-M" : Comp.SUB_DM,
    "A+D" : Comp.SUB_AD,
    "M-D" : Comp.SUB_MD,
    "D&A" : Comp.AND_DA,
    "D&M" : Comp.AND_DM,
    "D|A" : Comp.OR_DA,
    "D|M" : Comp.OR_DM  
}

jmp_asm = {
    "" : Jump.NULL,
    "JGT" : Jump.JGT,
    "JEQ" : Jump.JEQ,
    "JGE" : Jump.JGE,
    "JLT" : Jump.JLT,
    "JNE" : Jump.JNE,
    "JLE" : Jump.JLE,
    "JMP" : Jump.JMP
}

comp_table = {
    0b0101010 : Comp.ZERO,
    0b0111111 : Comp.ONE,
    0b0111010 : Comp.NEGONE,
    0b0001100 : Comp.D,
    0b0110000 : Comp.A,
    0b1110000 : Comp.M,
    0b0110001 : Comp.NOTA,
    0b1110001 : Comp.NOTM,
    0b0001111 : Comp.NEGD,
    0b0110011 : Comp.NEGA,
    0b1110011 : Comp.NEGM,
    0b0011111 : Comp.INCD,
    0b0110111 : Comp.INCA,
    0b1110111 : Comp.INCM,
    0b0001110 : Comp.DECD,
    0b0110010 : Comp.DECA,
    0b1110010 : Comp.DECM,
    0b0000010 : Comp.ADD_DA,
    0b1000010 : Comp.ADD_DM,
    0b0010011 : Comp.SUB_DA,
    0b1010011 : Comp.SUB_DM,
    0b0000111 : Comp.SUB_AD,
    0b1000111 : Comp.SUB_MD,
    0b0000000 : Comp.AND_DA,
    0b1000000 : Comp.AND_DM,
    0b0010101 : Comp.OR_DA,
    0b1010101 : Comp.OR_DM
    
}

jmp_table = {
    0b000 : Jump.NULL,
    0b001 : Jump.JGT,
    0b010 : Jump.JEQ,
    0b011 : Jump.JGE,
    0b100 : Jump.JLT,
    0b101 : Jump.JNE,
    0b110 : Jump.JLE,
    0b111 : Jump.JMP
}

dest_table = {
    0b000 : Dest.NULL,
    0b001 : Dest.M,
    0b010 : Dest.D,
    0b011 : Dest.MD,
    0b100 : Dest.A,
    0b101 : Dest.AM,
    0b110 : Dest.AD,
    0b111 : Dest.AMD
}


def lift_insn(insn):
    assert(0 <= insn < 2**16)
    jmp = 0b111 & insn
    dest = 0b111 & (insn >> 3)
    comp = 0b111111 & (insn >> 6)
    
    is_C_insn = (insn >> 15) == 1
    if is_C_insn:
        return CInsn(dest_table[dest], comp_table[comp], jmp_table[jmp])
    else:
        return AInsn(insn)
    

# Niceties for embedded DSL of Hack.
'''
class VarTable(dict):
    def __init__(self):
        self.vartable = {
            "R0" : 0,
            "R1" : 1,
            "R2" : 2
        }
        self.current = 16
    def __get_index__(self,name):
        if name is int:
            return AInsn(name)
        if name not in self.vartable:
            self.vartable[name] = current
            current += 1
        return AInsn(self.vartable[name])
'''

def At(x):
    return AInsn(x)
def C(dest, comp, *jmp):
    if len(jmp) == 0:
        jmp = Jump.NULL
    else:
        assert len(jmp) == 1
        jmp = jmp[0]
    if comp == 0:
        comp = Comp.ZERO
    elif comp == 1:
        comp = Comp.ONE
    elif comp == -1:
        comp = Comp.NEGONE
    elif comp == Dest.A:
        comp = Comp.A
    elif comp == Dest.D:
        comp = Comp.D
    elif comp == Dest.M:
        comp = Comp.M
    
    return CInsn(dest,comp,jmp)

def dest_add(self,rhs):
    if self == Dest.D and rhs == Dest.M:
        return Comp.ADD_DM
    elif self == Dest.D and rhs == Dest.A:
        return Comp.ADD_DA
    elif self == Dest.D and rhs == Dest.M:
        return Comp.ADD_DM
    elif self == D and rhs == 1:
        return Comp.INCD
    elif self == A and rhs == 1:
        return Comp.INCA
    if self == M and rhs == 1:
        return Comp.INCM
Dest.__add__ = dest_add
def dest_sub(self,rhs):
    if self == Dest.D and rhs == Dest.M:
        return Comp.SUB_DM
    elif self == Dest.D and rhs == Dest.A:
        return Comp.SUB_DA
    elif self == Dest.D and rhs == Dest.M:
        return Comp.SUB_DM
    elif self == D and rhs == 1:
        return Comp.DECD
    elif self == A and rhs == 1:
        return Comp.DECA
    if self == M and rhs == 1:
        return Comp.DECM
Dest.__sub__ = dest_sub

def disasm(text):
    return [lift_insn(int(insn,2)) for insn in text.split("\n")]



def asm(test):
    lines = text.split("\n")
    re.compile("@(?P<name>[A-Z]|d+)")
    for line in lines:
        line = line.split("//")[0]
        line.replace(' ', '')
        if line == '':
            pass
        elif line[0] == '@':
            AInstr(int(line[1:]))
        else:
            #((k,v),)= [k, v for k,v in dest_table.items() if line. ]
            dest, compjmp = line.split('=')
            compjmp = compjmp.split(';')
            comp = comp_table[compjmp[0]]
            
            if len(compjmp) == 2:
                jmp = compjmp[1]
            else:
                jmp = ""


            dest = dest_table[dest]
            comp = comp_table[comp]


        


prog = disasm(
'''0000000000001111
1110110000010000
1110011111001000
1111110111100000
1110110010010000
0000000000000000
1110101010000111''')




def jump_table(comp):
    return { 
    Jump.NULL: False,
    Jump.JGT: comp > 0,
    Jump.JEQ: comp == 0,
    Jump.JGE: comp >= 0,
    Jump.JLT: comp < 0,
    Jump.JNE: comp != 0,
    Jump.JLE: comp <= 0,
    Jump.JMP: True
    }

def comp_table(s):
    A = s.rA
    D = s.rD
    pc = s.pc
    M = s.mem(A)
    return {
        Comp.ZERO:  0,
        Comp.ONE: 1,
        Comp.NEGONE: -1,
        Comp.D: D,
        Comp.A: A,
        Comp.M: M,
        Comp.NOTD: ~D,
        Comp.NOTA: ~A,
        Comp.NOTM: ~M,
        Comp.NEGD: -D,
        Comp.NEGA: -A,
        Comp.NEGM: -M,
        Comp.INCD: D + 1,
        Comp.INCA: A + 1,
        Comp.INCM: M + 1,
        Comp.DECD: D - 1,
        Comp.DECA: A - 1,
        Comp.DECM: M - 1,
        Comp.ADD_DA: D + A,
        Comp.ADD_DM: D + M,
        Comp.SUB_DA: D - A,
        Comp.SUB_DM: D - M,
        Comp.SUB_AD: A - D,
        Comp.SUB_MD: M - D,
        Comp.AND_DA: D & A,
        Comp.AND_DM: D & M,
        Comp.OR_DA: D | A,
        Comp.OR_DM: D | M
    }

from z3 import *
BV16 = BitVecSort(16)
ramsize = 2
AddrSort = BitVecSort(ramsize)
A, D, PC, A1, D1, PC1 = BitVecs("A D PC A1 D1 PC1", BV16)

MEM = Array("RAM", AddrSort, BV16)
MEM1 = Array("RAM1", AddrSort, BV16)
RAM = lambda addr: MEM[Extract(ramsize-1,0,addr)]
RAM1 = lambda addr: MEM1[Extract(ramsize-1,0,addr)]


M = RAM(A)
M1 = RAM1(A1)

st = HackState(A,D,PC,RAM)
z3_comp_table = comp_table(st)

Insn = Function("Insn", BV16, BV16, BV16, ArraySort(AddrSort, BV16), BoolSort())
def z3_insn(insn):
    ''' Returns the transition relation corresponding to the given instruction'''
    if type(insn) == AInsn:
        return And([A1 == BitVecVal(insn.lit, 16), 
                    PC1 == PC + 1, 
                    D == D1, 
                    MEM == MEM1])
    elif type(insn) == CInsn:
        Trans = []

        comp1 = z3_comp_table[insn.comp]
        #newvals = [dest == comp1 for dest in [A1,D1] z3_dest_table[insn.dest]]
        if insn.dest in [Dest.A, Dest.AD, Dest.AD, Dest.AMD]:
            Trans += [A1 == comp1]
        else:
            Trans += [A1 == A]

        if insn.dest == Dest.D or insn.dest == Dest.MD or insn.dest == Dest.AD or insn.dest == Dest.AMD:
            Trans += [D1 == comp1]
        else:
            Trans += [D1 == D]

        if insn.dest == Dest.M or insn.dest == Dest.MD or insn.dest == Dest.AM or insn.dest == Dest.AMD:
            Trans += [MEM1 == Store(MEM, Extract(ramsize-1,0,A), comp1)]
        else:
            Trans += [MEM1 == MEM]
        
        cond = jump_table(comp1)[insn.jump]
        Trans += [If(cond, PC1 == A, PC1 == PC + 1)]
        
        return And(Trans)


def horn(prog):
    clauses = []
    Init = PC == 0
    # Program starts at pc = 0, arbitrary data in registers and ram
    clauses += [ForAll([A,A1,D,D1,PC,PC1,MEM,MEM1], Implies(And(Insn(A,D,PC,MEM),Init), False))]
    
    for pc, insn in enumerate(prog):
        # Build transition relation of instruction
        Trans = And(z3_insn(insn), PC == pc)
        clauses += [ForAll([A,A1,D,D1,PC,PC1,MEM,MEM1], 
                    Implies(And(Trans, Insn(A1,D1,PC1,MEM1)),
                            Insn(A,D,PC,MEM)))]
    return clauses

def assert_(prop):
    return ForAll([A, D, PC, MEM], Implies(Not(prop), Insn(A,D,PC,MEM) ))
