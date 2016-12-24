-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_shift_0 is
  port(w1     : in std_logic_vector(35 downto 0);
       ww     : in signed(63 downto 0);
       result : out std_logic_vector(35 downto 0));
end;

architecture structural of coreclash_shift_0 is
  signal app_arg      : signed(63 downto 0);
  signal app_arg_0    : signed(63 downto 0);
  signal app_arg_1    : signed(63 downto 0);
  signal case_scrut   : boolean;
  signal case_alt     : std_logic_vector(35 downto 0);
  signal app_arg_2    : signed(63 downto 0);
  signal app_arg_3    : signed(63 downto 0);
  signal case_scrut_0 : boolean;
  signal case_alt_0   : std_logic_vector(35 downto 0);
  signal case_alt_1   : std_logic_vector(35 downto 0);
begin
  app_arg <= to_signed(1,64) when ww < to_signed(0,64) else to_signed(0,64);
  
  app_arg_0 <= to_signed(1,64) when ww > to_signed(0,64) else to_signed(0,64);
  
  app_arg_1 <= ww;
  
  case_scrut <= tagToEnum(app_arg_0);
  
  case_alt <= std_logic_vector(shift_left(unsigned(w1),to_integer(app_arg_1)));
  
  app_arg_2 <= -ww;
  
  app_arg_3 <= app_arg_2;
  
  case_scrut_0 <= tagToEnum(app_arg);
  
  case_alt_0 <= case_alt when case_scrut else
                w1;
  
  case_alt_1 <= std_logic_vector(shift_right(unsigned(w1),to_integer(app_arg_3)));
  
  result <= case_alt_1 when case_scrut_0 else
            case_alt_0;
end;
