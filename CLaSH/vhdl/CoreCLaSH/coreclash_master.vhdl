-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.coreclash_types.all;

entity coreclash_master is
  port(ds1    : in coreclash_types.tup3;
       ins    : in coreclash_types.array_of_std_logic_vector_28(0 to 31);
       result : out coreclash_types.tup2_1);
end;

architecture structural of coreclash_master is
  signal result_0            : coreclash_types.tup2_1;
  signal result_1            : coreclash_types.tup3;
  signal app_arg             : coreclash_types.tup2_3;
  signal app_arg_0           : unsigned(5 downto 0);
  signal app_arg_1           : unsigned(4 downto 0);
  signal app_arg_2           : coreclash_types.array_of_signed_27(0 to 2);
  signal app_arg_3           : std_logic_vector(69 downto 0);
  signal qs                  : coreclash_types.array_of_signed_27(0 to 2);
  signal result_2            : coreclash_types.array_of_signed_27(0 to 2);
  signal ds2                 : coreclash_types.tup2_8;
  signal case_alt            : std_logic_vector(69 downto 0);
  signal pc                  : unsigned(5 downto 0);
  signal k                   : unsigned(4 downto 0);
  signal app_arg_4           : signed(63 downto 0);
  signal result_3            : signed(26 downto 0);
  signal ds2_case_alt        : coreclash_types.tup2_8;
  signal case_alt_0          : std_logic_vector(69 downto 0);
  signal ds2_0               : std_logic_vector(61 downto 0);
  signal result_4            : std_logic_vector(69 downto 0);
  signal case_scrut          : std_logic_vector(27 downto 0);
  signal wild1               : signed(63 downto 0);
  signal ds2_app_arg         : unsigned(5 downto 0);
  signal ds2_case_scrut      : boolean;
  signal ds2_case_alt_0      : coreclash_types.tup2_8;
  signal ds2_case_alt_1      : coreclash_types.tup2_8;
  signal case_alt_1          : std_logic_vector(69 downto 0);
  signal ds3                 : std_logic_vector(28 downto 0);
  signal instr_app_arg       : signed(63 downto 0);
  signal k_0                 : unsigned(4 downto 0);
  signal app_arg_5           : signed(63 downto 0);
  signal x                   : signed(26 downto 0);
  signal wild1_app_arg       : signed(63 downto 0);
  signal ds2_app_arg_0       : unsigned(4 downto 0);
  signal i                   : unsigned(5 downto 0);
  signal app_arg_6           : std_logic_vector(61 downto 0);
  signal a                   : unsigned(4 downto 0);
  signal wild1_0             : signed(63 downto 0);
  signal pc_0                : unsigned(5 downto 0);
  signal wild3               : signed(63 downto 0);
  signal a_0                 : unsigned(4 downto 0);
  signal app_arg_7           : std_logic_vector(28 downto 0);
  signal wild1_app_arg_0     : signed(63 downto 0);
  signal wild3_app_arg       : signed(63 downto 0);
  signal result_5            : signed(26 downto 0);
  signal app_arg_8           : signed(63 downto 0);
  signal wild1_1             : signed(63 downto 0);
  signal wild1_app_arg_1     : signed(63 downto 0);
  signal wild1_app_arg_2     : unsigned(4 downto 0);
  signal result_4_app_arg    : unsigned(4 downto 0);
  signal result_4_app_arg_0  : unsigned(4 downto 0);
  signal result_4_app_arg_1  : unsigned(4 downto 0);
  signal result_4_app_arg_2  : unsigned(4 downto 0);
  signal result_4_app_arg_3  : signed(26 downto 0);
  signal result_4_app_arg_4  : unsigned(4 downto 0);
  signal result_4_app_arg_5  : signed(26 downto 0);
  signal result_4_app_arg_6  : unsigned(4 downto 0);
  signal result_4_app_arg_7  : signed(26 downto 0);
  signal result_4_app_arg_8  : unsigned(4 downto 0);
  signal result_4_app_arg_9  : unsigned(4 downto 0);
  signal result_4_app_arg_10 : unsigned(4 downto 0);
  signal result_4_app_arg_11 : unsigned(4 downto 0);
  signal result_4_app_arg_12 : unsigned(4 downto 0);
  signal result_4_app_arg_13 : unsigned(4 downto 0);
  signal result_4_app_arg_14 : unsigned(4 downto 0);
  signal result_4_app_arg_15 : unsigned(4 downto 0);
  signal result_4_app_arg_16 : unsigned(4 downto 0);
  signal result_4_app_arg_17 : unsigned(4 downto 0);
  signal result_4_app_arg_18 : unsigned(4 downto 0);
  signal result_4_app_arg_19 : unsigned(4 downto 0);
  signal result_4_app_arg_20 : unsigned(4 downto 0);
  signal result_4_app_arg_21 : unsigned(4 downto 0);
  signal result_4_app_arg_22 : unsigned(4 downto 0);
  signal result_4_app_arg_23 : unsigned(4 downto 0);
  signal result_4_app_arg_24 : unsigned(4 downto 0);
  signal result_4_app_arg_25 : unsigned(4 downto 0);
  signal result_4_app_arg_26 : signed(26 downto 0);
  signal result_4_app_arg_27 : unsigned(4 downto 0);
  signal result_4_app_arg_28 : unsigned(4 downto 0);
  signal result_4_app_arg_29 : signed(26 downto 0);
  signal result_4_app_arg_30 : unsigned(4 downto 0);
  signal result_4_app_arg_31 : unsigned(4 downto 0);
  signal result_4_app_arg_32 : signed(26 downto 0);
  signal result_4_app_arg_33 : unsigned(4 downto 0);
  signal result_4_app_arg_34 : unsigned(4 downto 0);
  signal result_4_app_arg_35 : unsigned(4 downto 0);
  signal result_4_app_arg_36 : unsigned(4 downto 0);
  signal result_4_app_arg_37 : unsigned(4 downto 0);
  signal result_4_app_arg_38 : unsigned(4 downto 0);
  signal result_4_app_arg_39 : unsigned(4 downto 0);
  signal result_4_app_arg_40 : unsigned(4 downto 0);
  signal result_4_app_arg_41 : unsigned(4 downto 0);
  signal result_4_app_arg_42 : unsigned(4 downto 0);
  signal result_4_app_arg_43 : unsigned(4 downto 0);
  signal result_4_app_arg_44 : unsigned(4 downto 0);
  signal result_4_app_arg_45 : unsigned(4 downto 0);
  signal result_4_app_arg_46 : unsigned(4 downto 0);
  signal result_4_app_arg_47 : unsigned(4 downto 0);
  signal result_4_app_arg_48 : unsigned(4 downto 0);
  signal result_4_app_arg_49 : unsigned(4 downto 0);
  signal result_4_app_arg_50 : unsigned(4 downto 0);
  signal result_4_app_arg_51 : unsigned(5 downto 0);
  signal result_4_app_arg_52 : unsigned(4 downto 0);
  signal result_4_app_arg_53 : unsigned(4 downto 0);
  signal result_4_app_arg_54 : unsigned(4 downto 0);
  signal result_4_app_arg_55 : unsigned(4 downto 0);
  signal result_4_app_arg_56 : unsigned(4 downto 0);
  signal result_4_app_arg_57 : unsigned(4 downto 0);
  signal result_4_app_arg_58 : unsigned(4 downto 0);
  signal result_4_app_arg_59 : unsigned(4 downto 0);
  signal result_4_app_arg_60 : unsigned(4 downto 0);
  signal result_4_app_arg_61 : unsigned(4 downto 0);
  signal result_4_app_arg_62 : unsigned(4 downto 0);
  signal result_4_app_arg_63 : unsigned(4 downto 0);
  signal result_4_app_arg_64 : unsigned(4 downto 0);
  signal result_4_app_arg_65 : unsigned(4 downto 0);
  signal result_4_app_arg_66 : unsigned(4 downto 0);
  signal result_4_app_arg_67 : unsigned(4 downto 0);
  signal result_4_app_arg_68 : unsigned(4 downto 0);
  signal result_4_app_arg_69 : unsigned(4 downto 0);
  signal result_4_app_arg_70 : unsigned(4 downto 0);
  signal result_4_app_arg_71 : unsigned(4 downto 0);
  signal result_4_app_arg_72 : unsigned(4 downto 0);
  signal result_4_app_arg_73 : unsigned(4 downto 0);
  signal result_4_app_arg_74 : unsigned(4 downto 0);
  signal result_4_app_arg_75 : unsigned(4 downto 0);
  signal result_4_app_arg_76 : unsigned(4 downto 0);
  signal result_4_app_arg_77 : signed(26 downto 0);
  signal result_4_app_arg_78 : unsigned(4 downto 0);
  signal result_4_app_arg_79 : signed(26 downto 0);
  signal result_4_app_arg_80 : unsigned(4 downto 0);
  signal result_4_app_arg_81 : signed(26 downto 0);
begin
  result_0 <= (tup2_1_sel0 => result_1
              ,tup2_1_sel1 => app_arg);
  
  result_1 <= (tup3_sel0 => app_arg_0
              ,tup3_sel1 => app_arg_1
              ,tup3_sel2 => app_arg_2);
  
  app_arg <= (tup2_3_sel0 => app_arg_3
             ,tup2_3_sel1 => qs);
  
  app_arg_0 <= pc;
  
  app_arg_1 <= k;
  
  with (result_4(69 downto 67)) select
    app_arg_2 <= result_2 when "010",
                 qs when others;
  
  with (result_4(69 downto 67)) select
    app_arg_3 <= case_alt when "001",
                 result_4 when others;
  
  qs <= ds1.tup3_sel2;
  
  -- replace begin
  replacevec : block
    signal vec_index : integer range 0 to 3-1;
  begin
    vec_index <= to_integer(app_arg_4)
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
  
    process(vec_index,qs,result_3)
      variable ivec : coreclash_types.array_of_signed_27(0 to 2);
    begin
      ivec := qs;
      ivec(vec_index) := result_3;
      result_2 <= ivec;
    end process;
  end block;
  -- replace end
  
  with (result_4(69 downto 67)) select
    ds2 <= ds2_case_alt when "011",
           (tup2_8_sel0 => to_unsigned(0,6)
           ,tup2_8_sel1 => to_unsigned(0,5)) when "100",
           ds2_case_alt_0 when others;
  
  with (ds2_0(61 downto 61)) select
    case_alt <= case_alt_0 when "0",
                result_4 when others;
  
  pc <= ds2.tup2_8_sel0;
  
  k <= ds2.tup2_8_sel1;
  
  app_arg_4 <= wild1;
  
  with (case_scrut(27 downto 27)) select
    result_3 <= signed'(0 to 26 => 'X') when "0",
                x when others;
  
  ds2_case_alt <= ds2_case_alt_1 when ds2_case_scrut else
                  ds2_case_alt_0;
  
  with (ds3(28 downto 27)) select
    case_alt_0 <= case_alt_1 when "01",
                  result_4 when others;
  
  ds2_0 <= result_4(61 downto 0);
  
  result_4_app_arg <= to_unsigned(0,5);
  
  result_4_app_arg_0 <= to_unsigned(1,5);
  
  result_4_app_arg_1 <= to_unsigned(2,5);
  
  result_4_app_arg_2 <= to_unsigned(6,5);
  
  result_4_app_arg_3 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_4 <= to_unsigned(7,5);
  
  result_4_app_arg_5 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_6 <= to_unsigned(8,5);
  
  result_4_app_arg_7 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_8 <= to_unsigned(6,5);
  
  result_4_app_arg_9 <= to_unsigned(6,5);
  
  result_4_app_arg_10 <= to_unsigned(0,5);
  
  result_4_app_arg_11 <= to_unsigned(7,5);
  
  result_4_app_arg_12 <= to_unsigned(7,5);
  
  result_4_app_arg_13 <= to_unsigned(1,5);
  
  result_4_app_arg_14 <= to_unsigned(8,5);
  
  result_4_app_arg_15 <= to_unsigned(8,5);
  
  result_4_app_arg_16 <= to_unsigned(2,5);
  
  result_4_app_arg_17 <= to_unsigned(6,5);
  
  result_4_app_arg_18 <= to_unsigned(6,5);
  
  result_4_app_arg_19 <= to_unsigned(7,5);
  
  result_4_app_arg_20 <= to_unsigned(7,5);
  
  result_4_app_arg_21 <= to_unsigned(8,5);
  
  result_4_app_arg_22 <= to_unsigned(8,5);
  
  result_4_app_arg_23 <= to_unsigned(12,5);
  
  result_4_app_arg_24 <= to_unsigned(15,5);
  
  result_4_app_arg_25 <= to_unsigned(12,5);
  
  result_4_app_arg_26 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_27 <= to_unsigned(12,5);
  
  result_4_app_arg_28 <= to_unsigned(12,5);
  
  result_4_app_arg_29 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_30 <= to_unsigned(12,5);
  
  result_4_app_arg_31 <= to_unsigned(12,5);
  
  result_4_app_arg_32 <= to_signed(49152,27);
  
  result_4_app_arg_33 <= to_unsigned(13,5);
  
  result_4_app_arg_34 <= to_unsigned(12,5);
  
  result_4_app_arg_35 <= to_unsigned(13,5);
  
  result_4_app_arg_36 <= to_unsigned(13,5);
  
  result_4_app_arg_37 <= to_unsigned(14,5);
  
  result_4_app_arg_38 <= to_unsigned(13,5);
  
  result_4_app_arg_39 <= to_unsigned(14,5);
  
  result_4_app_arg_40 <= to_unsigned(6,5);
  
  result_4_app_arg_41 <= to_unsigned(9,5);
  
  result_4_app_arg_42 <= to_unsigned(9,5);
  
  result_4_app_arg_43 <= to_unsigned(14,5);
  
  result_4_app_arg_44 <= to_unsigned(7,5);
  
  result_4_app_arg_45 <= to_unsigned(10,5);
  
  result_4_app_arg_46 <= to_unsigned(10,5);
  
  result_4_app_arg_47 <= to_unsigned(14,5);
  
  result_4_app_arg_48 <= to_unsigned(8,5);
  
  result_4_app_arg_49 <= to_unsigned(11,5);
  
  result_4_app_arg_50 <= to_unsigned(11,5);
  
  result_4_app_arg_51 <= to_unsigned(0,6);
  
  result_4_app_arg_52 <= to_unsigned(16,5);
  
  result_4_app_arg_53 <= to_unsigned(9,5);
  
  result_4_app_arg_54 <= to_unsigned(3,5);
  
  result_4_app_arg_55 <= to_unsigned(3,5);
  
  result_4_app_arg_56 <= to_unsigned(16,5);
  
  result_4_app_arg_57 <= to_unsigned(10,5);
  
  result_4_app_arg_58 <= to_unsigned(4,5);
  
  result_4_app_arg_59 <= to_unsigned(4,5);
  
  result_4_app_arg_60 <= to_unsigned(16,5);
  
  result_4_app_arg_61 <= to_unsigned(11,5);
  
  result_4_app_arg_62 <= to_unsigned(5,5);
  
  result_4_app_arg_63 <= to_unsigned(5,5);
  
  result_4_app_arg_64 <= to_unsigned(16,5);
  
  result_4_app_arg_65 <= to_unsigned(3,5);
  
  result_4_app_arg_66 <= to_unsigned(0,5);
  
  result_4_app_arg_67 <= to_unsigned(0,5);
  
  result_4_app_arg_68 <= to_unsigned(16,5);
  
  result_4_app_arg_69 <= to_unsigned(4,5);
  
  result_4_app_arg_70 <= to_unsigned(1,5);
  
  result_4_app_arg_71 <= to_unsigned(1,5);
  
  result_4_app_arg_72 <= to_unsigned(16,5);
  
  result_4_app_arg_73 <= to_unsigned(5,5);
  
  result_4_app_arg_74 <= to_unsigned(2,5);
  
  result_4_app_arg_75 <= to_unsigned(2,5);
  
  result_4_app_arg_76 <= to_unsigned(9,5);
  
  result_4_app_arg_77 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_78 <= to_unsigned(10,5);
  
  result_4_app_arg_79 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  result_4_app_arg_80 <= to_unsigned(11,5);
  
  result_4_app_arg_81 <= shift_left(to_signed(0,27),to_integer(to_signed(15,64)));
  
  -- index begin
  indexvec : block 
  signal vec : coreclash_types.array_of_std_logic_vector_70(0 to 45);
  signal vec_index_0 : integer range 0 to 46-1;
  begin
    vec <= coreclash_types.array_of_std_logic_vector_70'(std_logic_vector'("010" & std_logic_vector(result_4_app_arg) & "00000000000000000000000000000000000000000000000000000000000000"),std_logic_vector'("010" & std_logic_vector(result_4_app_arg_0) & "00000000000000000000000000000000000000000000000000000000000000"),std_logic_vector'("010" & std_logic_vector(result_4_app_arg_1) & "00000000000000000000000000000000000000000000000000000000000000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_2) & std_logic_vector'("0" & std_logic_vector'("01" & std_logic_vector(result_4_app_arg_3)) & "00000000000000000000000000000000")),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_4) & std_logic_vector'("0" & std_logic_vector'("01" & std_logic_vector(result_4_app_arg_5)) & "00000000000000000000000000000000")),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_6) & std_logic_vector'("0" & std_logic_vector'("01" & std_logic_vector(result_4_app_arg_7)) & "00000000000000000000000000000000")),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_8) & std_logic_vector'("1" & std_logic_vector(to_unsigned(3,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_9) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_10) & "0000000000000000000000"))),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_11) & std_logic_vector'("1" & std_logic_vector(to_unsigned(3,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_12) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_13) & "0000000000000000000000"))),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_14) & std_logic_vector'("1" & std_logic_vector(to_unsigned(3,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_15) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_16) & "0000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_17) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_18) & "0000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_19) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_20) & "0000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("11" & "000000000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_21) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_22) & "0000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("11" & "000000000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_23) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("11" & "000000000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_24) & "0000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(0,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_25) & "0000000000000000000000") & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_26))) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_27) & std_logic_vector'("1" & std_logic_vector(to_unsigned(1,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_28) & "0000000000000000000000") & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_29)))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_30) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_31) & "0000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("11" & "000000000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000")) & "00000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(3,3)) & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_32)) & std_logic_vector'("11" & "000000000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_33) & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_34) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_35) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_36) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_37) & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("11" & "000000000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_38) & "0000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_39) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_40) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_41) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_42) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_43) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_44) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_45) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_46) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_47) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_48) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_49) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_50) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("011" & std_logic_vector(result_4_app_arg_51) & "0000000000000000000000000000000000000000000000000000000000000"),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_52) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_53) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_54) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_55) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_56) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_57) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_58) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_59) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_60) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_61) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_62) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_63) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_64) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_65) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_66) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_67) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_68) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_69) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_70) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_71) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("000" & std_logic_vector'("1" & std_logic_vector(to_unsigned(4,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_72) & "0000000000000000000000") & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_73) & "0000000000000000000000")) & "00000"),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_74) & std_logic_vector'("1" & std_logic_vector(to_unsigned(2,3)) & std_logic_vector'("10" & std_logic_vector(result_4_app_arg_75) & "0000000000000000000000") & std_logic_vector'("11" & "000000000000000000000000000"))),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_76) & std_logic_vector'("0" & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_77)) & "00000000000000000000000000000000")),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_78) & std_logic_vector'("0" & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_79)) & "00000000000000000000000000000000")),std_logic_vector'("001" & std_logic_vector(result_4_app_arg_80) & std_logic_vector'("0" & std_logic_vector'("00" & std_logic_vector(result_4_app_arg_81)) & "00000000000000000000000000000000")),std_logic_vector'("100" & "0000000000000000000000000000000000000000000000000000000000000000000"));
    vec_index_0 <= to_integer(instr_app_arg)
    -- pragma translate_off
                 mod 46
    -- pragma translate_on
                 ;
    result_4 <= vec(vec_index_0);
  end block;
  -- index end
  
  -- index begin
  indexvec_0 : block 
    signal vec_index_1 : integer range 0 to 32-1;
  begin
    vec_index_1 <= to_integer(app_arg_5)
    -- pragma translate_off
                 mod 32
    -- pragma translate_on
                 ;
    case_scrut <= ins(vec_index_1);
  end block;
  -- index end
  
  wild1 <= wild1_app_arg;
  
  ds2_app_arg <= pc_0 + to_unsigned(1,6);
  
  ds2_case_scrut <= k_0 /= to_unsigned(31,5);
  
  ds2_case_alt_0 <= (tup2_8_sel0 => ds2_app_arg
                    ,tup2_8_sel1 => k_0);
  
  ds2_case_alt_1 <= (tup2_8_sel0 => i
                    ,tup2_8_sel1 => ds2_app_arg_0);
  
  case_alt_1 <= std_logic_vector'("001" & std_logic_vector(a) & app_arg_6);
  
  ds3 <= ds2_0(60 downto 32);
  
  instr_app_arg <= wild1_0;
  
  k_0 <= ds1.tup3_sel1;
  
  app_arg_5 <= wild3;
  
  x <= signed(case_scrut(26 downto 0));
  
  wild1_app_arg <= signed(std_logic_vector(resize(a_0,64)));
  
  ds2_app_arg_0 <= k_0 + to_unsigned(1,5);
  
  i <= unsigned(result_4(66 downto 61));
  
  app_arg_6 <= std_logic_vector'("0" & app_arg_7 & "00000000000000000000000000000000");
  
  a <= unsigned(result_4(66 downto 62));
  
  wild1_0 <= wild1_app_arg_0;
  
  pc_0 <= ds1.tup3_sel0;
  
  wild3 <= wild3_app_arg;
  
  a_0 <= unsigned(result_4(66 downto 62));
  
  app_arg_7 <= std_logic_vector'("01" & std_logic_vector(result_5));
  
  wild1_app_arg_0 <= signed(std_logic_vector(resize(pc_0,64)));
  
  wild3_app_arg <= signed(std_logic_vector(resize(k_0,64)));
  
  -- index begin
  indexvec_1 : block 
    signal vec_index_2 : integer range 0 to 3-1;
  begin
    vec_index_2 <= to_integer(app_arg_8)
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    result_5 <= qs(vec_index_2);
  end block;
  -- index end
  
  app_arg_8 <= wild1_1;
  
  wild1_1 <= wild1_app_arg_1;
  
  wild1_app_arg_1 <= signed(std_logic_vector(resize(wild1_app_arg_2,64)));
  
  wild1_app_arg_2 <= a - to_unsigned(6,5);
  
  result <= result_0;
end;
