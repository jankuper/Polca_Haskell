-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_alu4 is
  port(l      : in std_logic_vector(0 downto 0);
       r      : in unsigned(4 downto 0);
       result : out unsigned(4 downto 0));
end;

architecture structural of coreclash_alu4 is
  signal case_scrut : boolean;
  signal case_alt   : unsigned(4 downto 0);
begin
  case_scrut <= l = (std_logic_vector'("0"));
  
  case_alt <= to_unsigned(1,5) + r;
  
  result <= case_alt when case_scrut else
            to_unsigned(0,5);
end;
