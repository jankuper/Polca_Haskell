-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_alu2 is
  port(x      : in signed(26 downto 0);
       result : out std_logic_vector(31 downto 0));
end;

architecture structural of coreclash_alu2 is
  signal app_arg      : std_logic_vector(22 downto 0);
  signal app_arg_0    : coreclash_types.array_of_std_logic_vector_1(0 to 22);
  signal app_arg_1    : coreclash_types.array_of_std_logic_vector_1(0 to 22);
  signal app_arg_2    : coreclash_types.array_of_std_logic_vector_1(0 to 53);
  signal app_arg_3    : coreclash_types.array_of_std_logic_vector_1(0 to 53);
  signal result_0     : std_logic_vector(53 downto 0);
  signal wild         : signed(63 downto 0);
  signal wild_app_arg : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  signal app_arg_4    : signed(63 downto 0);
  signal app_arg_5    : std_logic_vector(53 downto 0);
  signal app_arg_6    : std_logic_vector(26 downto 0);
  signal app_arg_7    : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  signal app_arg_8    : std_logic_vector(8 downto 0);
  signal app_arg_9    : unsigned(8 downto 0);
  signal app_arg_10   : unsigned(8 downto 0);
  signal app_arg_11   : unsigned(8 downto 0);
  signal app_arg_12   : signed(63 downto 0);
  signal result_1     : unsigned(4 downto 0);
  signal x1           : std_logic_vector(26 downto 0);
  signal app_arg_13   : coreclash_types.array_of_std_logic_vector_1(0 to 26);
begin
  result <= std_logic_vector'(std_logic_vector'(app_arg_8) & std_logic_vector'(app_arg));
  
  -- concatBitVector begin
  concatbitvector : block
    signal vec : coreclash_types.array_of_std_logic_vector_1(0 to 22);
  begin
    vec <= app_arg_0;
    concatbitvectoriter_loop : for i in vec'range generate
      app_arg(((i * 1) + 1 - 1) downto (i * 1)) <= std_logic_vector'(vec(vec'high - i));
    end generate;
  end block;
  -- concatBitVector end
  
  -- map begin
  map_r : for i_0 in app_arg_0'range generate
  begin
    coreclash_bitpackbitvector2_0 : entity coreclash_bitpackbitvector2
  port map
  (result => app_arg_0(i_0)
  ,v => app_arg_1(i_0));
  end generate;
  -- map end
  
  -- select begin
  select_r : for i_1 in app_arg_1'range generate
    app_arg_1(i_1) <= app_arg_2(12+(1*i_1));
  end generate;
  -- select end
  
  -- map begin
  map_r_1 : for i_2 in app_arg_2'range generate
  begin
    coreclash_bitpackbitvector1_1 : entity coreclash_bitpackbitvector1
  port map
  (result => app_arg_2(i_2)
  ,v => app_arg_3(i_2));
  end generate;
  -- map end
  
  -- unconcatBitVector begin
  unconcatbitvector : block
    signal vec_3 : std_logic_vector(53 downto 0);
  begin
    vec_3 <= result_0;
    unconcatbitvectoriter_loop : for i_3 in app_arg_3'range generate
      app_arg_3(app_arg_3'high - i_3) <= vec_3(((i_3 * 1) + 1 - 1) downto (i_3 * 1));
    end generate;
  end block;
  -- unconcatBitVector end
  
  coreclash_shift_result_0 : entity coreclash_shift
    port map
      (result => result_0
      ,w1     => app_arg_5
      ,ww     => app_arg_4);
  
  wild <= app_arg_12;
  
  -- unconcatBitVector begin
  unconcatbitvector_0 : block
    signal vec_4 : std_logic_vector(26 downto 0);
  begin
    vec_4 <= x1;
    unconcatbitvectoriter_loop_0 : for i_4 in wild_app_arg'range generate
      wild_app_arg(wild_app_arg'high - i_4) <= vec_4(((i_4 * 1) + 1 - 1) downto (i_4 * 1));
    end generate;
  end block;
  -- unconcatBitVector end
  
  app_arg_4 <= wild - to_signed(11,64);
  
  app_arg_5 <= std_logic_vector'(std_logic_vector'(x1) & std_logic_vector'(app_arg_6));
  
  -- concatBitVector begin
  concatbitvector_0 : block
    signal vec_5 : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  begin
    vec_5 <= app_arg_7;
    concatbitvectoriter_loop_0 : for i_5 in vec_5'range generate
      app_arg_6(((i_5 * 1) + 1 - 1) downto (i_5 * 1)) <= std_logic_vector'(vec_5(vec_5'high - i_5));
    end generate;
  end block;
  -- concatBitVector end
  
  -- map begin
  map_r_3 : block
    signal vec_6 : coreclash_types.array_of_std_logic_vector_1(0 to 26);
  begin
    vec_6 <= (coreclash_types.array_of_std_logic_vector_1'(0 to 27-1 =>  std_logic_vector'("0") ));
    map_r_4 : for i_6 in app_arg_7'range generate
    begin
      coreclash_bitpackbitvector2_2 : entity coreclash_bitpackbitvector2
  port map
  (result => app_arg_7(i_6)
  ,v => vec_6(i_6));
    end generate;
  end block;
  -- map end
  
  app_arg_8 <= std_logic_vector(app_arg_9);
  
  app_arg_9 <= to_unsigned(127,9) - app_arg_10;
  
  app_arg_10 <= app_arg_11 - to_unsigned(11,9);
  
  app_arg_11 <= resize(unsigned(std_logic_vector(app_arg_12)),9);
  
  app_arg_12 <= signed(std_logic_vector(resize(result_1,64)));
  
  -- foldr begin 
  foldr : block
    type foldr_res_type is array (natural range <>) of unsigned(4 downto 0);
    signal intermediate : foldr_res_type (0 to 27);
  begin
    intermediate(27) <= to_unsigned(0,5);
  
    foldr_loop : for i_7 in app_arg_13'range generate
    begin
      coreclash_alu4_3 : entity coreclash_alu4
  port map
  (result => intermediate(i_7)
  ,l => app_arg_13(i_7)
  ,r => intermediate(i_7+1));
    end generate;
  
    result_1 <= intermediate(0);
  end block;
  -- foldr end
  
  x1 <= std_logic_vector(x);
  
  -- map begin
  map_r_5 : for i_8 in app_arg_13'range generate
  begin
    coreclash_bitpackbitvector1_4 : entity coreclash_bitpackbitvector1
  port map
  (result => app_arg_13(i_8)
  ,v => wild_app_arg(i_8));
  end generate;
  -- map end
end;
