-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_alu is
  port(ds1    : in coreclash_types.tup3_0;
       expr   : in std_logic_vector(61 downto 0);
       result : out coreclash_types.tup2_6);
end;

architecture structural of coreclash_alu is
  signal result_0              : coreclash_types.tup2_6;
  signal result_1              : coreclash_types.tup2_6;
  signal result_2              : coreclash_types.tup2_6;
  signal result_3              : coreclash_types.tup2_6;
  signal result_4              : coreclash_types.tup2_6;
  signal case_alt              : coreclash_types.tup2_6;
  signal case_alt_0            : coreclash_types.tup2_6;
  signal opc                   : unsigned(2 downto 0);
  signal result_5              : coreclash_types.tup2_6;
  signal app_arg               : signed(26 downto 0);
  signal result_6              : signed(26 downto 0);
  signal app_arg_0             : signed(26 downto 0);
  signal app_arg_1             : signed(26 downto 0);
  signal app_arg_2             : signed(26 downto 0);
  signal app_arg_3             : unsigned(1 downto 0);
  signal calcelemout_app_arg   : coreclash_types.tup3_0;
  signal calcelemout_app_arg_0 : std_logic_vector(28 downto 0);
  signal app_arg_4             : signed(26 downto 0);
  signal app_arg_5             : std_logic_vector(31 downto 0);
  signal case_alt_1            : signed(26 downto 0);
  signal case_alt_2            : signed(26 downto 0);
  signal app_arg_6             : signed(26 downto 0);
  signal tupin                 : coreclash_types.tup2_7;
  signal sp0                   : unsigned(1 downto 0);
  signal app_arg_7             : std_logic_vector(31 downto 0);
  signal case_alt_3            : signed(26 downto 0);
  signal sp1                   : unsigned(1 downto 0);
  signal case_alt_4            : signed(26 downto 0);
  signal e0                    : std_logic_vector(28 downto 0);
  signal sel                   : coreclash_types.tup3_0;
  signal sel_0                 : std_logic_vector(28 downto 0);
  signal result_7              : coreclash_types.tup2_7;
  signal tupin_case_alt        : coreclash_types.tup2_7;
  signal val0                  : signed(26 downto 0);
  signal app_arg_8             : std_logic_vector(31 downto 0);
  signal val1                  : signed(26 downto 0);
  signal tupin_case_alt_0      : coreclash_types.tup2_7;
  signal e0_0                  : std_logic_vector(28 downto 0);
  signal x                     : signed(26 downto 0);
  signal tupin_app_arg         : coreclash_types.tup3_0;
  signal e1                    : std_logic_vector(28 downto 0);
  signal tupin_app_arg_0       : unsigned(1 downto 0);
  signal heap                  : coreclash_types.array_of_signed_27(0 to 16);
  signal stack                 : coreclash_types.array_of_signed_27(0 to 3);
  signal ds2                   : coreclash_types.tup2_6;
begin
  with (expr(61 downto 61)) select
    result_0 <= result_5 when "0",
                result_1 when others;
  
  with (opc) select
    result_1 <= case_alt when "000",
                case_alt_0 when "001",
                result_2 when "010",
                result_3 when "011",
                result_4 when others;
  
  result_2 <= (tup2_6_sel0 => app_arg_0
              ,tup2_6_sel1 => app_arg_3);
  
  result_3 <= (tup2_6_sel0 => app_arg_1
              ,tup2_6_sel1 => app_arg_3);
  
  result_4 <= (tup2_6_sel0 => app_arg_2
              ,tup2_6_sel1 => app_arg_3);
  
  case_alt <= (tup2_6_sel0 => app_arg
              ,tup2_6_sel1 => tupin_app_arg_0);
  
  case_alt_0 <= (tup2_6_sel0 => result_6
                ,tup2_6_sel1 => tupin_app_arg_0);
  
  opc <= unsigned(expr(60 downto 58));
  
  coreclash_calcelem_result_5 : entity coreclash_calcelem
    port map
      (result => result_5
      ,ds1    => calcelemout_app_arg
      ,expr   => calcelemout_app_arg_0);
  
  app_arg <= shift_right(app_arg_4,to_integer(to_signed(1,64)));
  
  coreclash_alu5_result_6 : entity coreclash_alu5
    port map
      (result    => result_6
      ,bitvector => app_arg_5);
  
  app_arg_0 <= case_alt_1;
  
  app_arg_1 <= case_alt_2;
  
  coreclash_ffractionalfixed_c_sffractionalfixed_csatmult_app_arg_2 : entity coreclash_ffractionalfixed_c_sffractionalfixed_csatmult
    port map
      (result => app_arg_2
      ,ds2    => x
      ,ds3    => app_arg_6);
  
  app_arg_3 <= sp1;
  
  calcelemout_app_arg <= sel;
  
  calcelemout_app_arg_0 <= sel_0;
  
  app_arg_4 <= val0;
  
  app_arg_5 <= std_logic_vector(unsigned((std_logic_vector(to_unsigned(1597463007,32)))) - unsigned(app_arg_7));
  
  case_alt_1 <= case_alt_3;
  
  case_alt_2 <= case_alt_4;
  
  app_arg_6 <= val1;
  
  with (expr(61 downto 61)) select
    tupin <= tupin_case_alt when "0",
             result_7 when others;
  
  sp0 <= ds2.tup2_6_sel1;
  
  app_arg_7 <= std_logic_vector(shift_right(unsigned(app_arg_8),to_integer(to_signed(1,64))));
  
  coreclash_ffractionalfixed_sfsaturatingnumsigned_csatplus_case_alt_3 : entity coreclash_ffractionalfixed_sfsaturatingnumsigned_csatplus
    port map
      (result => case_alt_3
      ,a      => val0
      ,b      => val1);
  
  sp1 <= result_5.tup2_6_sel1;
  
  coreclash_ffractionalfixed_sfsaturatingnumsigned_csatmin_case_alt_4 : entity coreclash_ffractionalfixed_sfsaturatingnumsigned_csatmin
    port map
      (result => case_alt_4
      ,a      => val0
      ,b      => val1);
  
  e0 <= expr(57 downto 29);
  
  sel <= tupin.tup2_7_sel0;
  
  sel_0 <= tupin.tup2_7_sel1;
  
  with (opc) select
    result_7 <= tupin_case_alt_0 when "010",
                tupin_case_alt_0 when "011",
                tupin_case_alt_0 when others;
  
  tupin_case_alt <= (tup2_7_sel0 => ds1
                    ,tup2_7_sel1 => e0_0);
  
  val0 <= ds2.tup2_6_sel0;
  
  coreclash_alu2_app_arg_8 : entity coreclash_alu2
    port map
      (result => app_arg_8
      ,x      => x);
  
  val1 <= result_5.tup2_6_sel0;
  
  tupin_case_alt_0 <= (tup2_7_sel0 => tupin_app_arg
                      ,tup2_7_sel1 => e1);
  
  e0_0 <= expr(60 downto 32);
  
  x <= val0;
  
  tupin_app_arg <= (tup3_0_sel0 => heap
                   ,tup3_0_sel1 => stack
                   ,tup3_0_sel2 => tupin_app_arg_0);
  
  e1 <= expr(28 downto 0);
  
  tupin_app_arg_0 <= sp0;
  
  heap <= ds1.tup3_0_sel0;
  
  stack <= ds1.tup3_0_sel1;
  
  coreclash_calcelem_ds2 : entity coreclash_calcelem
    port map
      (result => ds2
      ,ds1    => ds1
      ,expr   => e0);
  
  result <= result_0;
end;
