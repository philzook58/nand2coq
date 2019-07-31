module Ram8(
	input [15:0] in,
	input load,
	input clk,
	input [3:0] address,
	output reg [15:0] out
	);

reg [15:0] memory [0:7];

always @(posedge clk) begin
	if (load == 1'b1) begin
	 	memory[address] <= in;	
	end
	out <= memory[address];
end



endmodule