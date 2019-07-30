module RAM16K(in, address, load, out, clk);

input [0:15] in;
input [0:13] address;
input load;
input clk;
output reg [0:15] out;


reg [0:15] mem [0:16383];

always @(posedge clk) begin
	if (load == 1'b1)
	   mem[address] <= in;
	else
		out <= mem[address];
end

endmodule