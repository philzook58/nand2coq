# Verilog Implementation



I have made 2 versions. 
1. A Computer that is defined in `Computer2.v` in which every module is inlined and I take a more programmatic style.
2. A Sequence of modules that feels more like the nand2tetris presentation.

It may be nice to verify their equivalence. This verification does not lend all that much assurance to the total correctness of the implementation however.

You may compile a assembly program `myprog.asm`
```python
python assembly.py myprog
```

And then set the variable at the top of `Computer2.v` to `myprog.hack` (yes this is super janky)

And run the computer via:
```
iverilog Computer_tb.v Computer2.v
./a.out
```


ideas:
- Make a linker
- Make a dsl to compile to verilog
- Verify equivalence of different implementations
- Get running on Icestick
- Quickcheck or Brute force test using verilator. Equivalence to hack interpreter

<https://github.com/jopdorp/nand2tetris-verilog> Another implementation of nand2tetris cpu in verilog.
<https://gitlab.com/x653/nand2tetris-fpga/> another implementation of nand2teris in verilog


### Older notes



Fair warning: I believe where I left this off, the computer is not remotely near correct. In particular I still need to implement a fetch-execute clock division at least.


Description of files:
- Computer.v - top level file for hack computer
- alu.v -- the arithmetic logic unit
- ? 

The rough plan is to use a reasonable verilog implementation of the Nand2Tetris machine.
We can use verilog formal methods to verify tha these implemtnations match the output of the coq output.
I suspect that directly trying to use the coq output will not make the verilog compilers happy.

Amusingly, both coq and verilog use .v files.



# Verilog

- https://zipcpu.com/tutorial/
- http://asic-world.com/verilog/veritut.html

Verilog is built out of module units. Verilog is a Hardware Description Language (HDL).

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

blocking vs non blocking assignment
(=) is blocking, (<=) is non blocking.
(=) is more similar to the assignment operator in iperative programming languages. You can sequence them in the order you write them down.
(<=) is non blocking so the order you write them doesn't matter. They are more like a bunch of wires. Usually you'll want this operator in the always block.

### Initial Blocks

```verilog 

initial begin
    clk = 0;
    reset = 0;
    req_0 = 0;
    eq_1 = 0;
end
```

### Simulating

It is very important to be be able to run your circuits to actually see if they do the right thing

Icarus verilog is one such tool. You write test benches in verilog using funky consutrcts. I think we'll avoid

Using verilog as both your circuit descripition language and as your testbench language seems extra confusing to me. There are constructs in verilog that make absolutely no sense as circuits. We have tried to avoid mentioning any so far and let's just keep it that way.

Verilator is a verilog to C++ compiler. It will output C++ code that you can import into a C++ program. I don't love C++, but it is at least a relatively normal programming language.

https://www.veripool.org/wiki/verilator

`brew install verilator`


## Other


Getting Started with IceStick

http://www.clifford.at/icestorm/

on Ubuntu
`sudo apt-get install fpga-icetstorm`



Yosys SMT

https://symbiyosys.readthedocs.io/en/latest/index.html



Apio

IceStudio