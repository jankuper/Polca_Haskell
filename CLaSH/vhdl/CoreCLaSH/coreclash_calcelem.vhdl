-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_calcelem is
  port(ds1    : in coreclash_types.tup3_0;
       expr   : in std_logic_vector(28 downto 0);
       result : out coreclash_types.tup2_6);
end;

architecture structural of coreclash_calcelem is
  signal sp              : unsigned(1 downto 0);
  signal stack           : coreclash_types.array_of_signed_27(0 to 3);
  signal heap            : coreclash_types.array_of_signed_27(0 to 16);
  signal case_alt        : coreclash_types.tup2_6;
  signal app_arg         : unsigned(1 downto 0);
  signal app_arg_0       : signed(26 downto 0);
  signal a               : unsigned(4 downto 0);
  signal x               : signed(26 downto 0);
  signal n               : signed(26 downto 0);
  signal result_0        : signed(26 downto 0);
  signal wild1           : signed(63 downto 0);
  signal wild1_app_arg   : signed(63 downto 0);
  signal wild1_app_arg_0 : unsigned(1 downto 0);
  signal app_arg_1       : signed(63 downto 0);
  signal result_1        : signed(26 downto 0);
  signal wild1_0         : signed(63 downto 0);
  signal wild1_app_arg_1 : signed(63 downto 0);
  signal app_arg_2       : signed(63 downto 0);
begin
  result <= case_alt;
  
  sp <= ds1.tup3_0_sel2;
  
  stack <= ds1.tup3_0_sel1;
  
  heap <= ds1.tup3_0_sel0;
  
  case_alt <= (tup2_6_sel0 => app_arg_0
              ,tup2_6_sel1 => app_arg);
  
  with (expr(28 downto 27)) select
    app_arg <= wild1_app_arg_0 when "11",
               sp when others;
  
  with (expr(28 downto 27)) select
    app_arg_0 <= n when "00",
                 x when "01",
                 result_1 when "10",
                 result_0 when others;
  
  a <= unsigned(expr(26 downto 22));
  
  x <= signed(expr(26 downto 0));
  
  n <= signed(expr(26 downto 0));
  
  -- index begin
  indexvec : block 
    signal vec_index : integer range 0 to 4-1;
  begin
    vec_index <= to_integer(app_arg_1)
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    result_0 <= stack(vec_index);
  end block;
  -- index end
  
  wild1 <= wild1_app_arg;
  
  wild1_app_arg <= signed(std_logic_vector(resize(wild1_app_arg_0,64)));
  
  wild1_app_arg_0 <= sp - to_unsigned(1,2);
  
  app_arg_1 <= wild1;
  
  -- index begin
  indexvec_0 : block 
    signal vec_index_0 : integer range 0 to 17-1;
  begin
    vec_index_0 <= to_integer(app_arg_2)
    -- pragma translate_off
                 mod 17
    -- pragma translate_on
                 ;
    result_1 <= heap(vec_index_0);
  end block;
  -- index end
  
  wild1_0 <= wild1_app_arg_1;
  
  wild1_app_arg_1 <= signed(std_logic_vector(resize(a,64)));
  
  app_arg_2 <= wild1_0;
end;
