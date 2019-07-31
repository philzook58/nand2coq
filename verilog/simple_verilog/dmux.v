module mymux(in, sel, out1, out2);
   input in, sel;
   output out1, out2;
   assign out1 = in & sel;
   assign out2 = in & !sel;
endmodule