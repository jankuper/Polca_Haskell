-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_multicore is
  port(ds1    : in coreclash_types.tup2;
       inp    : in std_logic_vector(0 downto 0);
       result : out coreclash_types.tup2_0);
end;

architecture structural of coreclash_multicore is
  signal result_0       : coreclash_types.tup2_0;
  signal app_arg        : coreclash_types.tup2;
  signal app_arg_0      : coreclash_types.array_of_signed_27(0 to 2);
  signal app_arg_1      : coreclash_types.tup3;
  signal app_arg_2      : coreclash_types.array_of_tup3_0(0 to 31);
  signal ds2            : coreclash_types.tup3_1;
  signal qs             : coreclash_types.array_of_signed_27(0 to 2);
  signal ds2_case_scrut : coreclash_types.tup2_1;
  signal ds2_case_alt   : coreclash_types.tup3_1;
  signal xs             : coreclash_types.array_of_tup2_2(0 to 31);
  signal mstate         : coreclash_types.tup3;
  signal ds2_app_arg    : coreclash_types.array_of_std_logic_vector_28(0 to 31);
  signal ds2_case_alt_0 : coreclash_types.tup3_1;
  signal ds3            : coreclash_types.tup2_3;
  signal xs_app_arg     : coreclash_types.array_of_std_logic_vector_70(0 to 31);
  signal mstate_0       : coreclash_types.tup3;
  signal cstates        : coreclash_types.array_of_tup3_0(0 to 31);
  signal instr          : std_logic_vector(69 downto 0);
  signal qs_0           : coreclash_types.array_of_signed_27(0 to 2);
  signal mstate_1       : coreclash_types.tup3;
  signal xs_app_arg_0   : std_logic_vector(69 downto 0);
  signal instr_0        : std_logic_vector(69 downto 0);
begin
  result_0 <= (tup2_0_sel0 => app_arg
              ,tup2_0_sel1 => app_arg_0);
  
  app_arg <= (tup2_sel0 => app_arg_1
             ,tup2_sel1 => app_arg_2);
  
  app_arg_0 <= qs;
  
  app_arg_1 <= mstate;
  
  -- map begin
  map_r : for i in app_arg_2'range generate
  begin
    coreclash_fst_0 : entity coreclash_fst
  port map
  (result => app_arg_2(i)
  ,ds => xs(i));
  end generate;
  -- map end
  
  ds2 <= ds2_case_alt;
  
  qs <= ds2.tup3_1_sel2;
  
  coreclash_master_ds2_case_scrut : entity coreclash_master
    port map
      (result => ds2_case_scrut
      ,ds1    => mstate_0
      ,ins    => ds2_app_arg);
  
  ds2_case_alt <= ds2_case_alt_0;
  
  -- zipWith begin
  zipwith : for i_0 in xs'range generate
  begin
    coreclash_core_1 : entity coreclash_core
  port map
  (result => xs(i_0)
  ,ds1 => cstates(i_0)
  ,instr => xs_app_arg(i_0));
  end generate;
  -- zipWith end
  
  mstate <= ds2.tup3_1_sel0;
  
  -- map begin
  map_r_1 : for i_1 in ds2_app_arg'range generate
  begin
    coreclash_snd_2 : entity coreclash_snd
  port map
  (result => ds2_app_arg(i_1)
  ,ds => xs(i_1));
  end generate;
  -- map end
  
  ds2_case_alt_0 <= (tup3_1_sel0 => mstate_1
                    ,tup3_1_sel1 => instr
                    ,tup3_1_sel2 => qs_0);
  
  ds3 <= ds2_case_scrut.tup2_1_sel1;
  
  xs_app_arg <= coreclash_types.array_of_std_logic_vector_70'(0 to 32-1 =>  xs_app_arg_0 );
  
  mstate_0 <= ds1.tup2_sel0;
  
  cstates <= ds1.tup2_sel1;
  
  instr <= ds3.tup2_3_sel0;
  
  qs_0 <= ds3.tup2_3_sel1;
  
  mstate_1 <= ds2_case_scrut.tup2_1_sel0;
  
  xs_app_arg_0 <= instr_0;
  
  instr_0 <= ds2.tup3_1_sel1;
  
  result <= result_0;
end;
