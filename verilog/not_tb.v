module test;

  reg reset = 0;
  initial begin
     # 17 reset = 1;
     # 11 reset = 0;
     # 100 $stop;
  end
  wire value;
  not1  mynot (reset, value);
 initial
    $monitor("At time %t, value = %h (%0d)",
              $time, value, value);
endmodule // test