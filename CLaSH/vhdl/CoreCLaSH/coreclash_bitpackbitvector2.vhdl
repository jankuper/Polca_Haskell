-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_bitpackbitvector2 is
  port(v      : in std_logic_vector(0 downto 0);
       result : out std_logic_vector(0 downto 0));
end;

architecture structural of coreclash_bitpackbitvector2 is
begin
  result <= v;
end;
