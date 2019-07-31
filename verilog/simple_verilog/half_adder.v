module half_adder(
	ina, inb, sum, carry
	);

input ina;
input inb;
output sum;
output carry;
assign sum = ina ^ inb;
assign carry = ina & inb;

endmodule