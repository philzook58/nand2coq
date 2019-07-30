module alu(
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

wire [15:0] zerox;
wire [15:0] zeroy;
wire [15:0] notx;
wire [15:0] noty;
wire [15:0] andplus;

assign zerox = zx ? 0 : x;
assign notx = nx ? ~zerox : zerox;
assign zeroy = zy ? 0 : y;
assign noty = ny ? ~zeroy : zeroy;
assign andplus = f ? x + y : x & y;
assign out = no ? ~andplus : andplus; 

assign zr = out == 0;
assign ng = out[15] == 1; // check sign bit


endmodule