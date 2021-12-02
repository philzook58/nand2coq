from hack import *
A = Dest.A
D = Dest.D
M = Dest.M


Pos = 8
End = 10
signum = [
    At(R0),
    C(D,M),
    At(Pos),
    C(Dest.NULL, D, Jump.JGE),
    At(R1),
    C(M,-1),
    At(End),
    C(Dest.NULL,0,Jump.JMP),
]
assert Pos == len(signum)
signum += [
    At(R1),
    C(M,1)
]
assert End == len(signum)
signum += [
    At(End),
    C(Dest.NULL,0,Jump.JMP)
]
print(signum)


s = SolverFor("HORN")
s.add(horn(signum))
A, D, PC, A1, D1, PC1 = BitVecs("A D PC A1 D1 PC1", BV16)
s.add(assert_(Implies(PC == End, If(MEM[R0] >= 0, MEM[R1] == 1, MEM[R1] == -1))))
print(s)
status = s.check()
print(status)
if status == sat:
    print(s.model())