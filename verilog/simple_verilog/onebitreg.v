module Bit(
	input in,
	input load,
	input clk,
	output reg out

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