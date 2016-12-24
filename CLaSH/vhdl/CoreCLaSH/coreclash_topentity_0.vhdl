-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_topentity_0 is
  port(arg             : in std_logic_vector(0 downto 0);
       -- clock
       system1000      : in std_logic;
       -- asynchronous reset: active low
       system1000_rstn : in std_logic;
       result          : out coreclash_types.array_of_signed_27(0 to 2));
end;

architecture structural of coreclash_topentity_0 is
begin
  coreclash_topentity1_result : entity coreclash_topentity1
    port map
      (result          => result
      ,system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,eta2            => arg);
end;
