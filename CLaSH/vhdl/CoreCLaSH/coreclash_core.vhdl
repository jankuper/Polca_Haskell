-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_core is
  port(ds1    : in coreclash_types.tup3_0;
       instr  : in std_logic_vector(69 downto 0);
       result : out coreclash_types.tup2_2);
end;

architecture structural of coreclash_core is
  signal result_0        : coreclash_types.tup2_2;
  signal case_alt        : coreclash_types.tup2_2;
  signal case_alt_0      : coreclash_types.tup2_2;
  signal case_alt_1      : coreclash_types.tup2_2;
  signal case_alt_2      : coreclash_types.tup2_2;
  signal result_1        : coreclash_types.tup3_0;
  signal result_2        : coreclash_types.tup3_0;
  signal app_arg         : std_logic_vector(27 downto 0);
  signal result_3        : coreclash_types.array_of_signed_27(0 to 3);
  signal app_arg_0       : unsigned(1 downto 0);
  signal result_4        : coreclash_types.array_of_signed_27(0 to 16);
  signal result_5        : signed(26 downto 0);
  signal heap            : coreclash_types.array_of_signed_27(0 to 16);
  signal stack           : coreclash_types.array_of_signed_27(0 to 3);
  signal app_arg_1       : signed(63 downto 0);
  signal sp0             : unsigned(1 downto 0);
  signal app_arg_2       : signed(63 downto 0);
  signal app_arg_3       : signed(26 downto 0);
  signal app_arg_4       : signed(63 downto 0);
  signal wild1           : signed(63 downto 0);
  signal wild1_0         : signed(63 downto 0);
  signal sp0_0           : unsigned(1 downto 0);
  signal wild1_1         : signed(63 downto 0);
  signal aluout          : coreclash_types.tup2_6;
  signal val0            : signed(26 downto 0);
  signal wild1_app_arg   : signed(63 downto 0);
  signal wild1_app_arg_0 : signed(63 downto 0);
  signal wild1_app_arg_1 : signed(63 downto 0);
  signal aluout_app_arg  : std_logic_vector(61 downto 0);
  signal a               : unsigned(4 downto 0);
  signal a_0             : unsigned(4 downto 0);
  signal e0              : std_logic_vector(61 downto 0);
  signal e0_0            : std_logic_vector(61 downto 0);
begin
  with (instr(69 downto 67)) select
    result_0 <= case_alt_0 when "000",
                case_alt_1 when "001",
                case_alt_2 when "010",
                case_alt when others;
  
  case_alt <= (tup2_2_sel0 => ds1
              ,tup2_2_sel1 => std_logic_vector'("0" & "000000000000000000000000000"));
  
  case_alt_0 <= (tup2_2_sel0 => result_1
                ,tup2_2_sel1 => std_logic_vector'("0" & "000000000000000000000000000"));
  
  case_alt_1 <= (tup2_2_sel0 => result_2
                ,tup2_2_sel1 => std_logic_vector'("0" & "000000000000000000000000000"));
  
  case_alt_2 <= (tup2_2_sel0 => ds1
                ,tup2_2_sel1 => app_arg);
  
  result_1 <= (tup3_0_sel0 => heap
              ,tup3_0_sel1 => result_3
              ,tup3_0_sel2 => app_arg_0);
  
  result_2 <= (tup3_0_sel0 => result_4
              ,tup3_0_sel1 => stack
              ,tup3_0_sel2 => sp0);
  
  app_arg <= std_logic_vector'("1" & std_logic_vector(result_5));
  
  -- replace begin
  replacevec : block
    signal vec_index : integer range 0 to 4-1;
  begin
    vec_index <= to_integer(app_arg_1)
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
  
    process(vec_index,stack,app_arg_3)
      variable ivec : coreclash_types.array_of_signed_27(0 to 3);
    begin
      ivec := stack;
      ivec(vec_index) := app_arg_3;
      result_3 <= ivec;
    end process;
  end block;
  -- replace end
  
  app_arg_0 <= sp0 + to_unsigned(1,2);
  
  -- replace begin
  replacevec_0 : block
    signal vec_index_0 : integer range 0 to 17-1;
  begin
    vec_index_0 <= to_integer(app_arg_2)
    -- pragma translate_off
                 mod 17
    -- pragma translate_on
                 ;
  
    process(vec_index_0,heap,app_arg_3)
      variable ivec_0 : coreclash_types.array_of_signed_27(0 to 16);
    begin
      ivec_0 := heap;
      ivec_0(vec_index_0) := app_arg_3;
      result_4 <= ivec_0;
    end process;
  end block;
  -- replace end
  
  -- index begin
  indexvec : block 
    signal vec_index_1 : integer range 0 to 17-1;
  begin
    vec_index_1 <= to_integer(app_arg_4)
    -- pragma translate_off
                 mod 17
    -- pragma translate_on
                 ;
    result_5 <= heap(vec_index_1);
  end block;
  -- index end
  
  heap <= ds1.tup3_0_sel0;
  
  stack <= ds1.tup3_0_sel1;
  
  app_arg_1 <= wild1;
  
  sp0 <= sp0_0;
  
  app_arg_2 <= wild1_0;
  
  app_arg_3 <= val0;
  
  app_arg_4 <= wild1_1;
  
  wild1 <= wild1_app_arg;
  
  wild1_0 <= wild1_app_arg_0;
  
  sp0_0 <= aluout.tup2_6_sel1;
  
  wild1_1 <= wild1_app_arg_1;
  
  coreclash_alu_aluout : entity coreclash_alu
    port map
      (result => aluout
      ,ds1    => ds1
      ,expr   => aluout_app_arg);
  
  val0 <= aluout.tup2_6_sel0;
  
  wild1_app_arg <= signed(std_logic_vector(resize(sp0,64)));
  
  wild1_app_arg_0 <= signed(std_logic_vector(resize(a,64)));
  
  wild1_app_arg_1 <= signed(std_logic_vector(resize(a_0,64)));
  
  with (instr(69 downto 67)) select
    aluout_app_arg <= e0 when "000",
                      e0_0 when others;
  
  a <= unsigned(instr(66 downto 62));
  
  a_0 <= unsigned(instr(66 downto 62));
  
  e0 <= instr(66 downto 5);
  
  e0_0 <= instr(61 downto 0);
  
  result <= result_0;
end;
