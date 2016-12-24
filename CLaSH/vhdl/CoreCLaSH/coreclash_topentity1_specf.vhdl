-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_topentity1_specf is
  port(a1     : in coreclash_types.array_of_signed_27(0 to 16);
       ds1    : in coreclash_types.tup2_4;
       result : out coreclash_types.tup3_0);
end;

architecture structural of coreclash_topentity1_specf is
  signal case_alt : coreclash_types.tup3_0;
  signal b1       : coreclash_types.array_of_signed_27(0 to 3);
  signal c1       : unsigned(1 downto 0);
begin
  case_alt <= (tup3_0_sel0 => a1
              ,tup3_0_sel1 => b1
              ,tup3_0_sel2 => c1);
  
  b1 <= ds1.tup2_4_sel0;
  
  c1 <= ds1.tup2_4_sel1;
  
  result <= case_alt;
end;
