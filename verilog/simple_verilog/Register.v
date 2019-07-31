module Register(
	input [15:0] in,
	input load,
	input clk,
	output reg [15:0] out
	);


always @(posedge clk) begin
	if (load == 1'b1) begin
	 	out <= in;	
	end
	else begin
		out <= out;
	end
end

endmodule