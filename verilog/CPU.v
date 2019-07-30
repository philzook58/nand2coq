
// iverilog CPU.v PC.v alu.v
module CPU(instruction, 
	inM,
	 outM,
	  writeM,
	   PC,
	    addressM ,
	     reset,
	      clk);

input clk;
input reset;
output [0:14] addressM;
input [0:15] inM;
input [0:15] instruction;
output [0:15] outM;




output [0:15] PC;
output writeM;

// instruction: first bit is whther it is a load or not
// ixxacccccc
wire a;
wire i;
wire c1, c2, c3, c4, c5, c6;
wire j1,j2,j3;
// I think all these numbers are wrong. Backwards?
assign a = instruction[3];
assign i = instruction[0];
assign c1 = instruction[4];
assign c2 = instruction[5];
assign c3 = instruction[6];
assign c4 = instruction[7];
assign c5 = instruction[8];
assign c6 = instruction[9];

assign d1 = instruction[10]; // d1 controls loading into a
assign d2 = instruction[11]; // d2 loads into D
assign d3 = instruction[12]; // d3 loads into M (ram)

assign j1 = instruction[13];
assign j2 = instruction[14];
assign j3 = instruction[15];


// d3 controls writing memory only if C command
assign writeM = d3 & i;



reg [0:15] areg;
reg [0:15] dreg;

wire [0:15] aregIn;
wire [0:15] aluOut;
assign outM = aluOut;

wire aload;

wire [0:15] aluY;

// i bit picks whterh to input instruction or aluOutput
assign aregIn =  i ? instruction : aluOut; // Should I not reduce instruction?
assign aload =  !i | d1; // load the a reg if an A instruction or if that destinatin is set
assign aluY = a ? areg : inM; // The Y input to the alu is either A or M



assign addressM = areg;

// If the a register hsould be loaded
always @(posedge clk) begin
	if (aload == 1'b1)
		areg <= aregIn;
end

always @(posedge clk) begin
	if (d2 == 1'b1 && i == 1'b1)
		dreg <= aluOut;
end

wire zr;
wire ng;

alu myalu(.x (dreg), 
		.y (aluY),
		.out (aluOut),
		.zx (c1),
		.zy (c3),
		.nx (c2),
		.ny (c4),
		.f  (c5),
		.no (c6),
		.zr (zr),
		.ng (ng)
		);

// If you want to jump
wire pcload;
wire pcinc;
assign pcload = ((j2 & zr) | (j1 & ng) | (j2 & !ng)) & i; // only jump on C commands
assign pcinc = !pcload;
/*module alu(
	  input [15:0] x
	, input [15:0] y
	, output [15:0] out
	, input zx // zero x
	, input zy // zero y
	, input nx // negate result on x
	, input ny // """
	, input f // Plus is 1 or and if 0
	, input no // negate result?
	, output zr // is it exactly zero
	, output ng // is out < 0
	);
*/

PC mypc(
	.in (areg),
	.load (pcload),
	.inc (pcinc),
	.reset (reset),
	.clk (clk),
	.out (PC)
	);
/*
module PC(
	input [15:0] in,
	input load,
	input inc,
	input reset,
	input clk,
	output reg [15:0] out
	);
*/
endmodule