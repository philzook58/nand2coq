module tb();
reg clk, reset;
wire [15:0] PC;
wire [15:0] AReg;
wire [15:0] DReg;
wire [15:0] MReg;
wire [15:0] instruction;
Computer U1(clk, reset, PC, AReg, DReg, MReg, instruction);
always #5 clk <= ~clk;

initial begin
    $monitor("%t PC = %h A = %h D = %h M = %h I = %b", $time, PC, AReg, DReg, MReg, instruction);
    clk = 0;
    reset = 1;
    #20
    reset = 0;
    #100
    $finish;
end

endmodule

/*
So we have a verilog model.
We could make tests for the individual components.
That is kind of smart.

Quickcheck / Fuzzing makes sense. This would actually deliver a lot of confidence to me.
Brute force of all 2 instruction combos is feasible.
Definitely all 1 instruction.
1 instruction + random register / ram values.

We're not so much "proving it right" as we are trying to determine if there is any disparity between our two models.
The difficulty of "proving it right" is that it is hard to say really what it should do. There is always a specification gap.
This gap exists in pure mathematics as well. There is something (maybe) in the heads of mathemticians, and whether the formal system we build correctly reflects that is a matter of faith.

Write a C++ emulator.
Compile verilator version.

What I really want is a match between a model of the assembly language and the verilog.


Verilator becomes a part of our trusted code base
The C++ compiler is
The synthesis tools below verilog (which are complicated and reverse engineered)


I need an assembler


Modelling a subset of verilog directly.
There is not really a good reason to just target the module like language

type expr

Record module := {
    assigns : ;
    clocks  :  
}



So I had lots of little problems
I flipped some of the ?: cases backwards
I had left out declarations of some wires, which infers a single bit

I'm still not sure about endianness

*/