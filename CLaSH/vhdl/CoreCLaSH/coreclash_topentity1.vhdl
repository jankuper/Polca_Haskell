-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_topentity1 is
  port(eta2            : in std_logic_vector(0 downto 0);
       -- clock
       system1000      : in std_logic;
       -- asynchronous reset: active low
       system1000_rstn : in std_logic;
       result          : out coreclash_types.array_of_signed_27(0 to 2));
end;

architecture structural of coreclash_topentity1 is
  signal x_app_arg           : coreclash_types.array_of_array_of_17_signed_27(0 to 31);
  signal x_app_arg_0         : coreclash_types.array_of_tup3_0(0 to 31);
  signal x_app_arg_1         : coreclash_types.tup2;
  signal x                   : coreclash_types.array_of_signed_27(0 to 2);
  signal x_app_arg_0_app_arg : coreclash_types.array_of_tup2_4(0 to 31);
begin
  -- zipWith begin
  zipwith : block
    signal vec1 : coreclash_types.array_of_tup3_2(0 to 31);
    signal vec2 : coreclash_types.array_of_tup3_2(0 to 31);
  begin
    vec1 <= coreclash_types.array_of_tup3_2'((tup3_2_sel0 => -to_signed(32767,27),tup3_2_sel1 => -to_signed(24147,27),tup3_2_sel2 => to_signed(16751,27)),(tup3_2_sel0 => -to_signed(29684,27),tup3_2_sel1 => to_signed(11722,27),tup3_2_sel2 => to_signed(11750,27)),(tup3_2_sel0 => to_signed(21690,27),tup3_2_sel1 => -to_signed(30502,27),tup3_2_sel2 => -to_signed(29264,27)),(tup3_2_sel0 => -to_signed(7640,27),tup3_2_sel1 => -to_signed(28387,27),tup3_2_sel2 => -to_signed(5407,27)),(tup3_2_sel0 => to_signed(22686,27),tup3_2_sel1 => to_signed(1764,27),tup3_2_sel2 => -to_signed(26740,27)),(tup3_2_sel0 => to_signed(26890,27),tup3_2_sel1 => to_signed(17183,27),tup3_2_sel2 => -to_signed(15567,27)),(tup3_2_sel0 => to_signed(8692,27),tup3_2_sel1 => to_signed(16804,27),tup3_2_sel2 => to_signed(32180,27)),(tup3_2_sel0 => to_signed(14592,27),tup3_2_sel1 => to_signed(16603,27),tup3_2_sel2 => to_signed(9929,27)),(tup3_2_sel0 => -to_signed(14895,27),tup3_2_sel1 => -to_signed(4167,27),tup3_2_sel2 => to_signed(17465,27)),(tup3_2_sel0 => -to_signed(9223,27),tup3_2_sel1 => -to_signed(21855,27),tup3_2_sel2 => -to_signed(883,27)),(tup3_2_sel0 => to_signed(26519,27),tup3_2_sel1 => to_signed(296,27),tup3_2_sel2 => to_signed(1067,27)),(tup3_2_sel0 => -to_signed(15325,27),tup3_2_sel1 => -to_signed(26821,27),tup3_2_sel2 => to_signed(29344,27)),(tup3_2_sel0 => -to_signed(14609,27),tup3_2_sel1 => to_signed(27119,27),tup3_2_sel2 => to_signed(1949,27)),(tup3_2_sel0 => to_signed(17138,27),tup3_2_sel1 => to_signed(17708,27),tup3_2_sel2 => to_signed(21483,27)),(tup3_2_sel0 => to_signed(24133,27),tup3_2_sel1 => to_signed(8489,27),tup3_2_sel2 => to_signed(15481,27)),(tup3_2_sel0 => -to_signed(17485,27),tup3_2_sel1 => -to_signed(12692,27),tup3_2_sel2 => -to_signed(9763,27)),(tup3_2_sel0 => -to_signed(5761,27),tup3_2_sel1 => to_signed(22381,27),tup3_2_sel2 => -to_signed(15118,27)),(tup3_2_sel0 => -to_signed(13945,27),tup3_2_sel1 => -to_signed(21081,27),tup3_2_sel2 => -to_signed(22693,27)),(tup3_2_sel0 => to_signed(2257,27),tup3_2_sel1 => -to_signed(99,27),tup3_2_sel2 => to_signed(29842,27)),(tup3_2_sel0 => to_signed(8182,27),tup3_2_sel1 => to_signed(22415,27),tup3_2_sel2 => -to_signed(22297,27)),(tup3_2_sel0 => -to_signed(26804,27),tup3_2_sel1 => -to_signed(14772,27),tup3_2_sel2 => -to_signed(32571,27)),(tup3_2_sel0 => to_signed(28698,27),tup3_2_sel1 => -to_signed(17045,27),tup3_2_sel2 => -to_signed(20912,27)),(tup3_2_sel0 => -to_signed(22915,27),tup3_2_sel1 => to_signed(11884,27),tup3_2_sel2 => -to_signed(7483,27)),(tup3_2_sel0 => to_signed(5713,27),tup3_2_sel1 => to_signed(22647,27),tup3_2_sel2 => to_signed(5905,27)),(tup3_2_sel0 => to_signed(31673,27),tup3_2_sel1 => -to_signed(5979,27),tup3_2_sel2 => -to_signed(23473,27)),(tup3_2_sel0 => -to_signed(2357,27),tup3_2_sel1 => to_signed(30218,27),tup3_2_sel2 => -to_signed(24508,27)),(tup3_2_sel0 => -to_signed(24463,27),tup3_2_sel1 => to_signed(9912,27),tup3_2_sel2 => to_signed(7971,27)),(tup3_2_sel0 => -to_signed(7253,27),tup3_2_sel1 => -to_signed(19447,27),tup3_2_sel2 => -to_signed(30908,27)),(tup3_2_sel0 => to_signed(29326,27),tup3_2_sel1 => -to_signed(5877,27),tup3_2_sel2 => -to_signed(24170,27)),(tup3_2_sel0 => -to_signed(28110,27),tup3_2_sel1 => -to_signed(8825,27),tup3_2_sel2 => -to_signed(16183,27)),(tup3_2_sel0 => -to_signed(9861,27),tup3_2_sel1 => -to_signed(3126,27),tup3_2_sel2 => to_signed(20246,27)),(tup3_2_sel0 => to_signed(11769,27),tup3_2_sel1 => to_signed(26799,27),tup3_2_sel2 => -to_signed(16375,27)));
    vec2 <= coreclash_types.array_of_tup3_2'((tup3_2_sel0 => -to_signed(2709,27),tup3_2_sel1 => to_signed(2147,27),tup3_2_sel2 => -to_signed(18418,27)),(tup3_2_sel0 => to_signed(28488,27),tup3_2_sel1 => -to_signed(7634,27),tup3_2_sel2 => to_signed(1272,27)),(tup3_2_sel0 => to_signed(1946,27),tup3_2_sel1 => to_signed(11216,27),tup3_2_sel2 => -to_signed(32263,27)),(tup3_2_sel0 => to_signed(12240,27),tup3_2_sel1 => to_signed(5831,27),tup3_2_sel2 => to_signed(28209,27)),(tup3_2_sel0 => to_signed(10087,27),tup3_2_sel1 => -to_signed(5505,27),tup3_2_sel2 => to_signed(13185,27)),(tup3_2_sel0 => -to_signed(29657,27),tup3_2_sel1 => to_signed(15471,27),tup3_2_sel2 => -to_signed(11256,27)),(tup3_2_sel0 => -to_signed(8825,27),tup3_2_sel1 => -to_signed(16578,27),tup3_2_sel2 => to_signed(31624,27)),(tup3_2_sel0 => -to_signed(28004,27),tup3_2_sel1 => to_signed(8626,27),tup3_2_sel2 => to_signed(25212,27)),(tup3_2_sel0 => -to_signed(1459,27),tup3_2_sel1 => -to_signed(17185,27),tup3_2_sel2 => -to_signed(14751,27)),(tup3_2_sel0 => to_signed(26060,27),tup3_2_sel1 => to_signed(26817,27),tup3_2_sel2 => -to_signed(28798,27)),(tup3_2_sel0 => -to_signed(11859,27),tup3_2_sel1 => to_signed(31892,27),tup3_2_sel2 => -to_signed(394,27)),(tup3_2_sel0 => -to_signed(27934,27),tup3_2_sel1 => to_signed(46,27),tup3_2_sel2 => -to_signed(7592,27)),(tup3_2_sel0 => -to_signed(2330,27),tup3_2_sel1 => to_signed(28900,27),tup3_2_sel2 => -to_signed(29485,27)),(tup3_2_sel0 => -to_signed(24552,27),tup3_2_sel1 => -to_signed(31728,27),tup3_2_sel2 => to_signed(12350,27)),(tup3_2_sel0 => to_signed(14772,27),tup3_2_sel1 => to_signed(32732,27),tup3_2_sel2 => to_signed(25465,27)),(tup3_2_sel0 => to_signed(869,27),tup3_2_sel1 => to_signed(5971,27),tup3_2_sel2 => to_signed(22674,27)),(tup3_2_sel0 => -to_signed(5544,27),tup3_2_sel1 => to_signed(2444,27),tup3_2_sel2 => -to_signed(2102,27)),(tup3_2_sel0 => to_signed(4695,27),tup3_2_sel1 => to_signed(19818,27),tup3_2_sel2 => -to_signed(30601,27)),(tup3_2_sel0 => to_signed(16272,27),tup3_2_sel1 => to_signed(3577,27),tup3_2_sel2 => to_signed(25607,27)),(tup3_2_sel0 => -to_signed(18825,27),tup3_2_sel1 => to_signed(14071,27),tup3_2_sel2 => -to_signed(24220,27)),(tup3_2_sel0 => -to_signed(5616,27),tup3_2_sel1 => -to_signed(31006,27),tup3_2_sel2 => to_signed(13750,27)),(tup3_2_sel0 => -to_signed(11957,27),tup3_2_sel1 => to_signed(25361,27),tup3_2_sel2 => to_signed(9965,27)),(tup3_2_sel0 => -to_signed(7358,27),tup3_2_sel1 => -to_signed(16,27),tup3_2_sel2 => -to_signed(23099,27)),(tup3_2_sel0 => to_signed(29845,27),tup3_2_sel1 => to_signed(3679,27),tup3_2_sel2 => -to_signed(23058,27)),(tup3_2_sel0 => to_signed(4253,27),tup3_2_sel1 => -to_signed(16244,27),tup3_2_sel2 => -to_signed(752,27)),(tup3_2_sel0 => -to_signed(19676,27),tup3_2_sel1 => -to_signed(11845,27),tup3_2_sel2 => to_signed(8471,27)),(tup3_2_sel0 => to_signed(19862,27),tup3_2_sel1 => -to_signed(16525,27),tup3_2_sel2 => -to_signed(1544,27)),(tup3_2_sel0 => to_signed(26324,27),tup3_2_sel1 => -to_signed(4817,27),tup3_2_sel2 => -to_signed(23460,27)),(tup3_2_sel0 => to_signed(25273,27),tup3_2_sel1 => -to_signed(26727,27),tup3_2_sel2 => -to_signed(22138,27)),(tup3_2_sel0 => -to_signed(23913,27),tup3_2_sel1 => to_signed(18556,27),tup3_2_sel2 => -to_signed(2928,27)),(tup3_2_sel0 => to_signed(28290,27),tup3_2_sel1 => to_signed(9938,27),tup3_2_sel2 => -to_signed(18661,27)),(tup3_2_sel0 => to_signed(23649,27),tup3_2_sel1 => -to_signed(1883,27),tup3_2_sel2 => to_signed(390,27)));
    zipwith_0 : for i in x_app_arg'range generate
    begin
      coreclash_heaps0_stpl2heap_0 : entity coreclash_heaps0_stpl2heap
  port map
  (case_alt => x_app_arg(i)
  ,eta => vec1(i)
  ,eta1 => vec2(i));
    end generate;
  end block;
  -- zipWith end
  
  -- zipWith begin
  zipwith_1 : block
    signal vec1_0 : coreclash_types.array_of_array_of_4_signed_27(0 to 31);
    signal vec2_0 : coreclash_types.array_of_unsigned_2(0 to 31);
  begin
    vec1_0 <= (coreclash_types.array_of_array_of_4_signed_27'(0 to 32-1 =>  coreclash_types.array_of_signed_27'(shift_left(to_signed(0,27),to_integer(to_signed(15,64))),shift_left(to_signed(0,27),to_integer(to_signed(15,64))),shift_left(to_signed(0,27),to_integer(to_signed(15,64))),shift_left(to_signed(0,27),to_integer(to_signed(15,64)))) ));
    vec2_0 <= (coreclash_types.array_of_unsigned_2'(0 to 32-1 =>  to_unsigned(0,2) ));
    zipwith_2 : for i_0 in x_app_arg_0_app_arg'range generate
    begin
      x_app_arg_0_app_arg(i_0) <= (tup2_4_sel0 => vec1_0(i_0)
  ,tup2_4_sel1 => vec2_0(i_0));
    end generate;
  end block;
  -- zipWith end
  
  -- zipWith begin
  zipwith_3 : for i_1 in x_app_arg_0'range generate
  begin
    coreclash_topentity1_specf_1 : entity coreclash_topentity1_specf
  port map
  (result => x_app_arg_0(i_1)
  ,a1 => x_app_arg(i_1)
  ,ds1 => x_app_arg_0_app_arg(i_1));
  end generate;
  -- zipWith end
  
  x_app_arg_1 <= (tup2_sel0 => (tup3_sel0 => to_unsigned(0,6)
                               ,tup3_sel1 => to_unsigned(0,5)
                               ,tup3_sel2 => coreclash_types.array_of_signed_27'(shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                                                ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))
                                                                                ,shift_left(to_signed(0,27),to_integer(to_signed(15,64)))))
                 ,tup2_sel1 => x_app_arg_0);
  
  coreclash_mealy_x : entity coreclash_mealy
    port map
      (result          => x
      ,system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,w1              => x_app_arg_1
      ,w2              => eta2);
  
  result <= x;
end;
