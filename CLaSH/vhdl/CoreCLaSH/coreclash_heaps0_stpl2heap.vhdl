-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_heaps0_stpl2heap is
  port(eta      : in coreclash_types.tup3_2;
       eta1     : in coreclash_types.tup3_2;
       case_alt : out coreclash_types.array_of_signed_27(0 to 16));
end;

architecture structural of coreclash_heaps0_stpl2heap is
  signal pz        : signed(26 downto 0);
  signal py        : signed(26 downto 0);
  signal px        : signed(26 downto 0);
  signal vz        : signed(26 downto 0);
  signal vy        : signed(26 downto 0);
  signal vx        : signed(26 downto 0);
  signal app_arg   : coreclash_types.array_of_signed_27(0 to 15);
  signal app_arg_0 : coreclash_types.array_of_signed_27(0 to 14);
  signal app_arg_1 : coreclash_types.array_of_signed_27(0 to 13);
  signal app_arg_2 : coreclash_types.array_of_signed_27(0 to 12);
  signal app_arg_3 : coreclash_types.array_of_signed_27(0 to 11);
begin
  pz <= eta.tup3_2_sel2;
  
  py <= eta.tup3_2_sel1;
  
  px <= eta.tup3_2_sel0;
  
  vz <= eta1.tup3_2_sel2;
  
  vy <= eta1.tup3_2_sel1;
  
  vx <= eta1.tup3_2_sel0;
  
  case_alt <= coreclash_types.array_of_signed_27'(signed'(px) & app_arg);
  
  app_arg <= coreclash_types.array_of_signed_27'(signed'(py) & app_arg_0);
  
  app_arg_0 <= coreclash_types.array_of_signed_27'(signed'(pz) & app_arg_1);
  
  app_arg_1 <= coreclash_types.array_of_signed_27'(signed'(vx) & app_arg_2);
  
  app_arg_2 <= coreclash_types.array_of_signed_27'(signed'(vy) & app_arg_3);
  
  app_arg_3 <= coreclash_types.array_of_signed_27'(vz
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                  ,to_signed(32,27)
                                                  ,to_signed(1310,27));
end;
