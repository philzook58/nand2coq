module full_adder(
	ina, inb, inc, sum, carry
	);

input ina;
input inb;
input inc;
output sum;
output carry;
wire c;
wire c2;
half_adder add1 (ina, inb, s, c);
half_adder add2 (c, inc,  sum, c2);
assign carry = c2 | c;

endmodule