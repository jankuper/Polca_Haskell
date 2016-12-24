-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_topentity is
  port(input_0         : in std_logic_vector(0 downto 0);
       -- clock
       system1000      : in std_logic;
       -- asynchronous reset: active low
       system1000_rstn : in std_logic;
       output_0_0      : out signed(26 downto 0);
       output_0_1      : out signed(26 downto 0);
       output_0_2      : out signed(26 downto 0));
end;

architecture structural of coreclash_topentity is
  signal output_0 : coreclash_types.array_of_signed_27(0 to 2);
begin
  coreclash_topentity_0_inst : entity coreclash_topentity_0
    port map
      (arg             => input_0
      ,system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,result          => output_0);
  
  output_0_0 <= output_0(0);
  
  output_0_1 <= output_0(1);
  
  output_0_2 <= output_0(2);
end;
