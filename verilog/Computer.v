module Computer(reset, clk);
 input reset;
 input clk;

 CPU mycpu(.instruction (instruction),
 	.inM (inM),
 	.outM (outM),
 	.writeM (writeM),
 	.PC (PC),
 	.addressM (addressM),
 	.reset (reset),
 	.clk (clk)

 	);


wire [0:15] addressM;
wire [0:15] outM ;
wire [0:15] inM ;
wire [0:15] instruction;
wire writeM;
wire [0:15] PC;

ROM32K rom(.address (PC), .out (instruction));
Memory memory(.in (outM),
       .load (writeM),
       .address (addressM),
       .out (inM),
       .clk (clk) );

/*
module Memory(in, load, address, out, clk );
*/

/*
 module CPU(instruction, 
	inM,
	 outM,
	  writeM,
	   PC,
	    addressM ,
	     reset,
	      clk);
8?

endmodule