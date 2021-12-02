from hack import *

End = 8
prog = [
AInsn(R0),
CInsn( Dest.D, Comp.M, Jump.NULL),
AInsn(R1),
CInsn( Dest.D, Comp.ADD_DM, Jump.NULL),
AInsn(17),
CInsn( Dest.D, Comp.ADD_DA, Jump.NULL),
AInsn(R2),
CInsn( Dest.M, Comp.D, Jump.NULL),
AInsn(End),
CInsn( Dest.NULL, Comp.ZERO, Jump.JMP),
]
# prog itself could perhaps profitably be made a class
# with a prog.add
# or a prog.C, prog.A
# prog.label("End")

A = Dest.A
D = Dest.D
M = Dest.M

prog = [
    At(R0),
    C(D, M),
    At(R1),
    C( D, D + M),
    At(17),
    C(D, D + A),
    At(R2),
    C( M, D)]
End = len(prog)
prog += [
    At(End),
    C( Dest.NULL, 0, Jump.JMP),
]

s = SolverFor("HORN")
s.add(horn(prog))
#s.add(assert_(PC <= 9))
A, D, PC, A1, D1, PC1 = BitVecs("A D PC A1 D1 PC1", BV16)
#s.add(assert_(Implies(PC == 4, D == MEM[1] + MEM[0])))
#s.add(assert_(Implies(PC == 4, D == MEM[1] + MEM[0])))
#s.add(assert_(Implies(PC => 5, D == MEM[2])))
#s.add(assert_(Implies(PC >= 6, D == MEM[1] + MEM[0] + 17)))
s.add(assert_(Implies(PC == 8, MEM[R2] == MEM[R1] + MEM[R0] + 17)))#RAM(BitVecVal(2, BV16)) == RAM(BitVecVal(2, BV16))  )))#MEM[1] + MEM[0] + 17)))
print(s)
status = s.check()
print(status)
if status == sat:
    print(s.model())
