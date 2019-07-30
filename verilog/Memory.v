module Memory(in, load, address, out, clk );

input clk;
input [0:15] in;
input load;
input [0:14] address;
output reg [0:15] out;

wire [0:15] kbdout;
wire [0:15] ramout;
wire [0:15] screenout;
//wire [0:15] 


reg ramload;
reg screenload;

always @(address or load) begin
	if (address <= 16'h3FFF) begin
		out = ramout;
		ramload = load;
		screenload = 1'b0;
	end else if (address <= 16'h5FFF) begin
		out = 0; 
		ramload = 1'b0;
		screenload = load;
	end else begin
		out = kbdout;
		ramload = 1'b0;
		screenload = 1'b0;
	end
end 


/*
reg [0:15] ram16K [0:13];

always @posedge(clk) begin
	
end
*/

RAM16K ram16(in, address, ramload ,ramout, clk);
//SCREEN myscreen(in, address[0:13], screenload, screenout, clk);
//KEYBOARD mykbd(kbdout, clk);
endmodule