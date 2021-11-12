`define PROGRAMFILE "setd.hack"

module Computer(input clk, input reset, output [0:15] PC, output [0:15] AReg,
            output [0:15] DReg, output [0:15] MReg, output [0:15] instruction);

reg [0:15] PC; // Current Program Counter
reg [0:15] AReg; // A register
reg [0:15] DReg; // D Register
wire [0:15] MReg; // M Register. A Pseudo register that is actuall the output of current index of ram
reg [0:15] ram[0:16383];
reg [0:15] rom[0:16383];

wire [0:15] instruction;
wire writeA; // Write to A
wire writeD; // Write to D
wire writeM; // Write to M
wire pcload; // Write to PC
wire [0:15] aluOut;

////////////////////////////
// Program Counter
// If you want to jump
assign pcload = ((j2 & zr) | (j1 & ng) | (j2 & !ng)) & i; // only jump on C commands

always @(posedge clk) begin
	if (reset == 1'b1)
		PC <= 16'b0;
	else if (pcload == 1'b1)
	    PC <= AReg;
	else
	    PC <= PC + 1;
end
//////////////////////////

//// ROM /////////////
assign instruction = rom[PC];
initial begin
    $readmemb(`PROGRAMFILE, rom);
end
/////////////////////

//// RAM ////////////
assign MReg = ram[AReg];
always @(posedge clk) begin
    // Also clear RAM on reset?
	if (writeM == 1'b1)
	   ram[AReg] <= aluOut;
end
/////////////////////

wire a, i, zx, zy, nx, ny, f, no;
wire d1, d2, d3;
wire j1, j2, j3;

/// CPU///////////
// Instruction bit interpretations
assign i  = instruction[0];  // instruction type. 0 = load immediate into AReg, 1 = compute alu function C command
assign a  = instruction[3];  // a = 0 use A as operand, if a = 1 use M as operand
assign zx = instruction[4]; // x is zero
assign nx = instruction[5]; // negate x
assign zy = instruction[6]; // y is zero
assign ny = instruction[7]; // negate y
assign f  = instruction[8]; // select addition vs. bitwise and
assign no = instruction[9]; // negate output

assign d1 = instruction[10]; // d1 controls loading into a
assign d2 = instruction[11]; // d2 loads into D
assign d3 = instruction[12]; // d3 loads into M (ram)

assign j1 = instruction[13];
assign j2 = instruction[14];
assign j3 = instruction[15];

// If the A register should be written
wire [0:15] aregIn;
assign aregIn =  i ? aluOut : instruction;
assign writeA =  !i | d1; // load the A reg if an A instruction or if that destination is set in C instruction
always @(posedge clk) begin
	if (writeA == 1'b1)
		AReg <= aregIn;
end

// If the D register should be written.
assign writeD = i & d2;
always @(posedge clk) begin
	if (writeD == 1'b1)
		DReg <= aluOut;
end

// If the M register should be written
assign writeM = i & d3;
/////////////

/// ALU /////////////
wire [0:15] x;
wire [0:15] y;
wire [0:15] zerox;
wire [0:15] zeroy;
wire [0:15] notx;
wire [0:15] noty;
wire [0:15] andplus;



assign y = a ? MReg : AReg; // The Y input to the alu is either A or M
assign x = DReg;

// Table 2.6
assign zerox = zx ? 16'h0000 : x;
assign notx = nx ? ~zerox : zerox;
assign zeroy = zy ? 16'h0000 : y;
assign noty = ny ? ~zeroy : zeroy;
assign andplus = f ? (notx + noty) : (notx & noty);
assign aluOut = no ? ~andplus : andplus;

assign zr = aluOut == 16'h0000;
assign ng = aluOut[15] == 1; // check sign bit? Is this the right number?
/////////////////////

endmodule
