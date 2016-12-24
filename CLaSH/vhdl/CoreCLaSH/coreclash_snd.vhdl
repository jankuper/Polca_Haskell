-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_snd is
  port(ds     : in coreclash_types.tup2_2;
       result : out std_logic_vector(27 downto 0));
end;

architecture structural of coreclash_snd is
  signal y : std_logic_vector(27 downto 0);
begin
  y <= ds.tup2_2_sel1;
  
  result <= y;
end;
