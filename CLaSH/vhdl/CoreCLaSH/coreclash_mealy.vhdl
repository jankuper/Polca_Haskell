-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_mealy is
  port(w1              : in coreclash_types.tup2;
       w2              : in std_logic_vector(0 downto 0);
       -- clock
       system1000      : in std_logic;
       -- asynchronous reset: active low
       system1000_rstn : in std_logic;
       result          : out coreclash_types.array_of_signed_27(0 to 2));
end;

architecture structural of coreclash_mealy is
  signal y         : coreclash_types.array_of_signed_27(0 to 2);
  signal result_0  : coreclash_types.tup2_0;
  signal x         : coreclash_types.tup2;
  signal x_app_arg : coreclash_types.tup2;
  signal x_0       : coreclash_types.tup2;
begin
  result <= y;
  
  y <= result_0.tup2_0_sel1;
  
  coreclash_multicore_result_0 : entity coreclash_multicore
    port map
      (result => result_0
      ,ds1    => x
      ,inp    => w2);
  
  -- register begin
  coreclash_mealy_register : process(system1000,system1000_rstn,w1)
  begin
    if system1000_rstn = '0' then
      x <= w1;
    elsif rising_edge(system1000) then
      x <= x_app_arg;
    end if;
  end process;
  -- register end
  
  x_app_arg <= x_0;
  
  x_0 <= result_0.tup2_0_sel0;
end;
