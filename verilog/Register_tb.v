module Register_tb();


reg clk;
reg [15:0] in;
reg load;
wire [15:0] out;


Register myreg(in, load, clk, out);
initial begin
	clk = 0;
	in = 0;
	load = 1;
	//out = 0;
#2
   in = 20;
#2
load = 0;
#2
in = 30;
#4
load = 1;
#2

$finish;
end


 always 
   #1  clk =  ! clk;

initial begin
$monitor("in=%d,load=%d,clk=%d, out=%d \n",in,load, clk, out);
end
endmodule