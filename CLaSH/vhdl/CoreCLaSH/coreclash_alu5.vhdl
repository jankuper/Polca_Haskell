-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_alu5 is
  port(bitvector : in std_logic_vector(31 downto 0);
       result    : out signed(26 downto 0));
end;

architecture structural of coreclash_alu5 is
  signal app_arg        : coreclash_types.array_of_std_logic_vector_1(0 to 22);
  signal app_arg_0      : coreclash_types.array_of_std_logic_vector_1(0 to 23);
  signal app_arg_1      : coreclash_types.array_of_std_logic_vector_1(0 to 24);
  signal app_arg_2      : coreclash_types.array_of_std_logic_vector_1(0 to 35);
  signal app_arg_3      : coreclash_types.array_of_std_logic_vector_1(0 to 35);
  signal app_arg_4      : std_logic_vector(35 downto 0);
  signal wild_app_arg   : coreclash_types.array_of_std_logic_vector_1(0 to 7);
  signal wild_app_arg_0 : coreclash_types.array_of_std_logic_vector_1(0 to 7);
  signal wild_app_arg_1 : std_logic_vector(7 downto 0);
  signal wild_app_arg_2 : std_logic_vector(7 downto 0);
  signal wild_app_arg_3 : signed(7 downto 0);
  signal wild_app_arg_4 : signed(63 downto 0);
  signal wild           : signed(63 downto 0);
  signal result_0       : std_logic_vector(35 downto 0);
  signal vector_app_arg : coreclash_types.array_of_std_logic_vector_1(0 to 31);
  signal vector         : coreclash_types.array_of_std_logic_vector_1(0 to 31);
  signal app_arg_5      : coreclash_types.array_of_std_logic_vector_1(0 to 35);
  signal app_arg_6      : coreclash_types.array_of_std_logic_vector_1(0 to 35);
  signal app_arg_7      : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  signal app_arg_8      : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  signal app_arg_9      : std_logic_vector(26 downto 0);
begin
  -- select begin
  select_r : for i in app_arg'range generate
    app_arg(i) <= vector(9+(1*i));
  end generate;
  -- select end
  
  app_arg_0 <= coreclash_types.array_of_std_logic_vector_1'(coreclash_types.array_of_std_logic_vector_1'(app_arg) & coreclash_types.array_of_std_logic_vector_1'((coreclash_types.array_of_std_logic_vector_1'(0 to 1-1 =>  std_logic_vector'("0") ))));
  
  app_arg_1 <= coreclash_types.array_of_std_logic_vector_1'(coreclash_types.array_of_std_logic_vector_1'((coreclash_types.array_of_std_logic_vector_1'(0 to 1-1 =>  std_logic_vector'("1") ))) & coreclash_types.array_of_std_logic_vector_1'(app_arg_0));
  
  app_arg_2 <= coreclash_types.array_of_std_logic_vector_1'(coreclash_types.array_of_std_logic_vector_1'((coreclash_types.array_of_std_logic_vector_1'(0 to 11-1 =>  std_logic_vector'("0") ))) & coreclash_types.array_of_std_logic_vector_1'(app_arg_1));
  
  -- map begin
  map_r : for i_0 in app_arg_3'range generate
  begin
    coreclash_bitpackbitvector2_0 : entity coreclash_bitpackbitvector2
  port map
  (result => app_arg_3(i_0)
  ,v => app_arg_2(i_0));
  end generate;
  -- map end
  
  -- concatBitVector begin
  concatbitvector : block
    signal vec_1 : coreclash_types.array_of_std_logic_vector_1(0 to 35);
  begin
    vec_1 <= app_arg_3;
    concatbitvectoriter_loop : for i_1 in vec_1'range generate
      app_arg_4(((i_1 * 1) + 1 - 1) downto (i_1 * 1)) <= std_logic_vector'(vec_1(vec_1'high - i_1));
    end generate;
  end block;
  -- concatBitVector end
  
  -- select begin
  select_r_1 : for i_2 in wild_app_arg'range generate
    wild_app_arg(i_2) <= vector(1+(1*i_2));
  end generate;
  -- select end
  
  -- map begin
  map_r_1 : for i_3 in wild_app_arg_0'range generate
  begin
    coreclash_bitpackbitvector2_1 : entity coreclash_bitpackbitvector2
  port map
  (result => wild_app_arg_0(i_3)
  ,v => wild_app_arg(i_3));
  end generate;
  -- map end
  
  -- concatBitVector begin
  concatbitvector_0 : block
    signal vec_4 : coreclash_types.array_of_std_logic_vector_1(0 to 7);
  begin
    vec_4 <= wild_app_arg_0;
    concatbitvectoriter_loop_0 : for i_4 in vec_4'range generate
      wild_app_arg_1(((i_4 * 1) + 1 - 1) downto (i_4 * 1)) <= std_logic_vector'(vec_4(vec_4'high - i_4));
    end generate;
  end block;
  -- concatBitVector end
  
  wild_app_arg_2 <= std_logic_vector(unsigned(wild_app_arg_1) - unsigned(std_logic_vector'(x"7F")));
  
  wild_app_arg_3 <= signed(wild_app_arg_2);
  
  wild_app_arg_4 <= resize(wild_app_arg_3,64);
  
  wild <= wild_app_arg_4;
  
  coreclash_shift_0_result_0 : entity coreclash_shift_0
    port map
      (result => result_0
      ,w1     => app_arg_4
      ,ww     => wild);
  
  -- unconcatBitVector begin
  unconcatbitvector : block
    signal vec_5 : std_logic_vector(31 downto 0);
  begin
    vec_5 <= bitvector;
    unconcatbitvectoriter_loop : for i_5 in vector_app_arg'range generate
      vector_app_arg(vector_app_arg'high - i_5) <= vec_5(((i_5 * 1) + 1 - 1) downto (i_5 * 1));
    end generate;
  end block;
  -- unconcatBitVector end
  
  -- map begin
  map_r_3 : for i_6 in vector'range generate
  begin
    coreclash_bitpackbitvector1_2 : entity coreclash_bitpackbitvector1
  port map
  (result => vector(i_6)
  ,v => vector_app_arg(i_6));
  end generate;
  -- map end
  
  -- unconcatBitVector begin
  unconcatbitvector_0 : block
    signal vec_7 : std_logic_vector(35 downto 0);
  begin
    vec_7 <= result_0;
    unconcatbitvectoriter_loop_0 : for i_7 in app_arg_5'range generate
      app_arg_5(app_arg_5'high - i_7) <= vec_7(((i_7 * 1) + 1 - 1) downto (i_7 * 1));
    end generate;
  end block;
  -- unconcatBitVector end
  
  -- map begin
  map_r_5 : for i_8 in app_arg_6'range generate
  begin
    coreclash_bitpackbitvector1_3 : entity coreclash_bitpackbitvector1
  port map
  (result => app_arg_6(i_8)
  ,v => app_arg_5(i_8));
  end generate;
  -- map end
  
  -- select begin
  select_r_3 : for i_9 in app_arg_7'range generate
    app_arg_7(i_9) <= app_arg_6(0+(1*i_9));
  end generate;
  -- select end
  
  -- map begin
  map_r_7 : for i_10 in app_arg_8'range generate
  begin
    coreclash_bitpackbitvector2_4 : entity coreclash_bitpackbitvector2
  port map
  (result => app_arg_8(i_10)
  ,v => app_arg_7(i_10));
  end generate;
  -- map end
  
  -- concatBitVector begin
  concatbitvector_1 : block
    signal vec_11 : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  begin
    vec_11 <= app_arg_8;
    concatbitvectoriter_loop_1 : for i_11 in vec_11'range generate
      app_arg_9(((i_11 * 1) + 1 - 1) downto (i_11 * 1)) <= std_logic_vector'(vec_11(vec_11'high - i_11));
    end generate;
  end block;
  -- concatBitVector end
  
  result <= signed(app_arg_9);
end;
