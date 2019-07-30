module onebit_tb;

reg in;
reg load;
reg clk;
wire out;

Bit myreg(in, load, clk, out);


initial begin
 in = 0;
 load = 0;
 clk = 0;
 //out = 0;
 #10
 load = 1;
 in = 1;
 #10
 clk = 1;
 #10
 load = 0;
 in = 0;
 #10 
 clk = 0;
 #10
 clk = 1;


end

initial begin
$monitor("in=%d,load=%d,clk=%d, out=%d \n",in,load, clk, out);
end
endmodule

