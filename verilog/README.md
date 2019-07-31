# Verilog Implementation


Fair warning: I believe where I left this off, the computer is not near correct. In particular I still need to implement a fetch-execute clock division at least.


Description of files:
- Computer.v - top level file for hack computer
- alu.v -- the arithmetic logic unit
- ? 

The rough plan is to use a reasonable verilog implementation of the Nand2Tetris machine.
We can use verilog formal methods to verify tha these implemtnations match the output of the coq output.
I suspect that directly trying to use the coq output will not make the verilog compilers happy.

Amusingly, both coq and verilog use .v files.


Apio

IceStudio

Verilog Resources

https://zipcpu.com/tutorial/

Verilog is built of of module units. Verilog is a Hardware Description Language (HDL).

Here is a simple not gate module

```verilog
module myNot(i,o);
   input wire i;
   output wire o;

   assign  o = !i;
endmodule
```

Combinatorial circuitry can be created via assign statements.

```verilog
module myNand(i1, i2, o);
   input wire i1, i2;
   output wire o;

   assign o = i1 & i2;
endmodule
```

.pcf files describe the connection of your toplevel module to the pins on the fpga.
The icestick has some useful pins to know about.
Clock, leds, serial ports.


You can make a bus. This is useful as we'll often be working with bitvectors. The following is defining an 8 bit bus.
`input wire [7 : 0] fred;`


Literals
Preface with the size of the bus. b for binary, h for hex, d for decimal
1'b0
2'b01
16'h0

Operators

bitwise operators

folds are prefix operators



We will need a clock signal and timing elements. In verilog





Getting Started with IceStick

http://www.clifford.at/icestorm/

on Ubuntu
`sudo apt-get install fpga-icetstorm`

Verilator

https://www.veripool.org/wiki/verilator

Yosys SMT

https://symbiyosys.readthedocs.io/en/latest/index.html

