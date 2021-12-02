// Sets R2 to R0 + R1 + 17
// D = R0
@R0
D=M
//D = D + R1
@R1
D=D+M
//D = D + 17
@17
D=D+A
//R2 = D
@R2
M=D
(End)
@End
0;JMP