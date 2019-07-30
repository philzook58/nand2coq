module alu_tb;

reg [15:0] x;
reg [15:0] y;
reg zx,zy,nx,ny,f,no;
wire [15:0]out;
wire zr,ng;

alu myalu(x,y,out,zx,zy,nx,ny,f,no, zr,ng );

initial begin
    $dumpfile("test.vcd");
    $dumpvars(0,out);
	x = 0;
	y = 0;
	zx = 0;
	zy = 0;
	nx = 0;
	ny = 0;
	f = 0;
	no = 0;
	#10
	x = 255;
	//f = 0;
	y = 5;
	#10;
	f =  1;
	#10;
	ny = 1;
	f = 0;
	#10;
	ny = 0;
	no = 1;
	#10;
	no = 0;
	//$display(out);
	//$display(zr);
	#10;
end
initial begin
$monitor("x=%d,y=%d,out=%d \n",x,y, out);
end
endmodule