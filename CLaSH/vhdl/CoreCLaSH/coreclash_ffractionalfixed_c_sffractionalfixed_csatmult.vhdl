-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_ffractionalfixed_c_sffractionalfixed_csatmult is
  port(ds2    : in signed(26 downto 0);
       ds3    : in signed(26 downto 0);
       result : out signed(26 downto 0));
end;

architecture structural of coreclash_ffractionalfixed_c_sffractionalfixed_csatmult is
  signal ww2          : std_logic_vector(41 downto 0);
  signal ww1          : std_logic_vector(11 downto 0);
  signal case_alt     : signed(26 downto 0);
  signal case_scrut   : coreclash_types.tup2_9;
  signal case_alt_0   : signed(26 downto 0);
  signal case_alt_1   : signed(26 downto 0);
  signal case_scrut_0 : std_logic_vector(0 downto 0);
  signal app_arg      : std_logic_vector(26 downto 0);
  signal app_arg_0    : std_logic_vector(41 downto 0);
  signal case_scrut_1 : std_logic_vector(0 downto 0);
  signal app_arg_1    : std_logic_vector(0 downto 0);
  signal app_arg_2    : std_logic_vector(0 downto 0);
  signal app_arg_3    : std_logic_vector(0 downto 0);
  signal app_arg_4    : std_logic_vector(12 downto 0);
  signal app_arg_5    : std_logic_vector(0 downto 0);
  signal app_arg_6    : std_logic_vector(53 downto 0);
  signal app_arg_7    : signed(53 downto 0);
begin
  result <= case_alt;
  
  ww2 <= case_scrut.tup2_9_sel1;
  
  ww1 <= case_scrut.tup2_9_sel0;
  
  with (case_scrut_0) select
    case_alt <= case_alt_0 when "1",
                case_alt_1 when others;
  
  -- split begin
  case_scrut <= (app_arg_6(app_arg_6'high downto 42)
             ,app_arg_6(42-1 downto 0)
             );
  -- split end
  
  case_alt_0 <= signed(app_arg);
  
  with (case_scrut_1) select
    case_alt_1 <= signed'(0 => '0', 1 to 27-1  => '1') when "0",
                  signed'(0 => '1', 1 to 27-1 => '0') when others;
  
  case_scrut_0 <= app_arg_2 or app_arg_1;
  
  app_arg <= std_logic_vector(resize(unsigned(app_arg_0),27));
  
  app_arg_0 <= std_logic_vector(shift_right(unsigned(ww2),to_integer(to_signed(15,64))));
  
  -- msb begin 
  case_scrut_1 <= ww1(ww1'high downto ww1'high);
  -- msb end
  
  -- reduceAnd begin
  reduceand : block
    function and_reduce (arg : std_logic_vector) return std_logic is
      variable upper, lower : std_logic;
      variable half         : integer;
      variable argi         : std_logic_vector (arg'length - 1 downto 0);
      variable result       : std_logic;
    begin
      if (arg'length < 1) then
        result := '1';
      else
        argi := arg;
        if (argi'length = 1) then
          result := argi(argi'left);
        else
          half   := (argi'length + 1) / 2; -- lsb-biased tree
          upper  := and_reduce (argi (argi'left downto half));
          lower  := and_reduce (argi (half - 1 downto argi'right));
          result := upper and lower;
        end if;
      end if;
      return result;
    end;
  begin
    app_arg_1 <= (0 => and_reduce(app_arg_4));
  end block;
  -- reduceAnd end
  
  app_arg_2 <= not app_arg_3;
  
  -- reduceOr begin
  reduceor : block
    function or_reduce (arg : std_logic_vector) return std_logic is
      variable upper, lower : std_logic;
      variable half         : integer;
      variable argi         : std_logic_vector (arg'length - 1 downto 0);
      variable result       : std_logic;
    begin
      if (arg'length < 1) then
        result := '0';
      else
        argi := arg;
        if (argi'length = 1) then
          result := argi(argi'left);
        else
          half   := (argi'length + 1) / 2; -- lsb-biased tree
          upper  := or_reduce (argi (argi'left downto half));
          lower  := or_reduce (argi (half - 1 downto argi'right));
          result := upper or lower;
        end if;
      end if;
      return result;
    end;
  begin
    app_arg_3 <= (0 => or_reduce(app_arg_4));
  end block;
  -- reduceOr end
  
  app_arg_4 <= std_logic_vector'(std_logic_vector'(app_arg_5) & std_logic_vector'(ww1));
  
  -- msb begin 
  app_arg_5 <= ww2(ww2'high downto ww2'high);
  -- msb end
  
  app_arg_6 <= std_logic_vector(app_arg_7);
  
  app_arg_7 <= ds2 * ds3;
end;
