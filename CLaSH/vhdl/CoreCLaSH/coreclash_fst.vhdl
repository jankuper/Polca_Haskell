-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_fst is
  port(ds     : in coreclash_types.tup2_2;
       result : out coreclash_types.tup3_0);
end;

architecture structural of coreclash_fst is
  signal x : coreclash_types.tup3_0;
begin
  x <= ds.tup2_2_sel0;
  
  result <= x;
end;
