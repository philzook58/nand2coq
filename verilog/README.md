# Verilog Implementation



The rough plan is to use a reasonable verilog implementation of the Nand2Tetris machine.
We can use verilog formal methods to verify tha these implemtnations match the output of the coq output.
I suspect that directly trying to use the coq output will not make the verilog compilers happy.

Amusingly, both coq and verilog use .v files.

Fair warning: I believe where I left this off, the computer is not near correct. In particular I still need to implement a fetch-execute clock division at least.