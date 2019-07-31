module myor(i1, i2, o);
   input i1, i2;
   output o;
 // always
   wire in1, in2;
   not1 fred(i1, in1);
   not1 larry(i2, in2);
   assign o = in1 & in2;


endmodule