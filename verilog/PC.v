module PC(
	input [15:0] in,
	input load,
	input inc,
	input reset,
	input clk,
	output reg [15:0] out
	);

always @(posedge clk) begin
	if (reset == 1'b0)
		out <= 16'b0;
	else if (load == 1'b1)
	    out <= in;
	else if (inc == 1'b1)
	    out <= out + 1;
end


endmodule