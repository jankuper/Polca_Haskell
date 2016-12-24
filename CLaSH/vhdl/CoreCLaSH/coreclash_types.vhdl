library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package coreclash_types is
  type array_of_signed_27 is array (integer range <>) of signed(26 downto 0);
  type array_of_std_logic_vector_1 is array (integer range <>) of std_logic_vector(0 downto 0);
  type tup3_0 is record
    tup3_0_sel0 : coreclash_types.array_of_signed_27(0 to 16);
    tup3_0_sel1 : coreclash_types.array_of_signed_27(0 to 3);
    tup3_0_sel2 : unsigned(1 downto 0);
  end record;
  type tup2_2 is record
    tup2_2_sel0 : coreclash_types.tup3_0;
    tup2_2_sel1 : std_logic_vector(27 downto 0);
  end record;
  type array_of_tup2_2 is array (integer range <>) of coreclash_types.tup2_2;
  type tup2_7 is record
    tup2_7_sel0 : coreclash_types.tup3_0;
    tup2_7_sel1 : std_logic_vector(28 downto 0);
  end record;
  type tup2_9 is record
    tup2_9_sel0 : std_logic_vector(11 downto 0);
    tup2_9_sel1 : std_logic_vector(41 downto 0);
  end record;
  type tup2_4 is record
    tup2_4_sel0 : coreclash_types.array_of_signed_27(0 to 3);
    tup2_4_sel1 : unsigned(1 downto 0);
  end record;
  type array_of_tup2_4 is array (integer range <>) of coreclash_types.tup2_4;
  type tup3_2 is record
    tup3_2_sel0 : signed(26 downto 0);
    tup3_2_sel1 : signed(26 downto 0);
    tup3_2_sel2 : signed(26 downto 0);
  end record;
  type array_of_std_logic_vector_70 is array (integer range <>) of std_logic_vector(69 downto 0);
  type array_of_unsigned_2 is array (integer range <>) of unsigned(1 downto 0);
  type tup2_5 is record
    tup2_5_sel0 : std_logic_vector(0 downto 0);
    tup2_5_sel1 : std_logic_vector(26 downto 0);
  end record;
  type array_of_array_of_4_signed_27 is array (integer range <>) of coreclash_types.array_of_signed_27(0 to 3);
  type tup2_8 is record
    tup2_8_sel0 : unsigned(5 downto 0);
    tup2_8_sel1 : unsigned(4 downto 0);
  end record;
  type tup2_3 is record
    tup2_3_sel0 : std_logic_vector(69 downto 0);
    tup2_3_sel1 : coreclash_types.array_of_signed_27(0 to 2);
  end record;
  type tup2_6 is record
    tup2_6_sel0 : signed(26 downto 0);
    tup2_6_sel1 : unsigned(1 downto 0);
  end record;
  type array_of_std_logic_vector_28 is array (integer range <>) of std_logic_vector(27 downto 0);
  type tup3 is record
    tup3_sel0 : unsigned(5 downto 0);
    tup3_sel1 : unsigned(4 downto 0);
    tup3_sel2 : coreclash_types.array_of_signed_27(0 to 2);
  end record;
  type array_of_tup3_0 is array (integer range <>) of coreclash_types.tup3_0;
  type tup2 is record
    tup2_sel0 : coreclash_types.tup3;
    tup2_sel1 : coreclash_types.array_of_tup3_0(0 to 31);
  end record;
  type tup2_0 is record
    tup2_0_sel0 : coreclash_types.tup2;
    tup2_0_sel1 : coreclash_types.array_of_signed_27(0 to 2);
  end record;
  type tup2_1 is record
    tup2_1_sel0 : coreclash_types.tup3;
    tup2_1_sel1 : coreclash_types.tup2_3;
  end record;
  type array_of_array_of_17_signed_27 is array (integer range <>) of coreclash_types.array_of_signed_27(0 to 16);
  type array_of_tup3_2 is array (integer range <>) of coreclash_types.tup3_2;
  type tup3_1 is record
    tup3_1_sel0 : coreclash_types.tup3;
    tup3_1_sel1 : std_logic_vector(69 downto 0);
    tup3_1_sel2 : coreclash_types.array_of_signed_27(0 to 2);
  end record;
  function toSLV (s : in signed) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_signed_27) return std_logic_vector;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_1) return std_logic_vector;
  function toSLV (u : in unsigned) return std_logic_vector;
  function toSLV (p : coreclash_types.tup3_0) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_2) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_tup2_2) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_7) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_9) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_4) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_tup2_4) return std_logic_vector;
  function toSLV (p : coreclash_types.tup3_2) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_70) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_unsigned_2) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_5) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_array_of_4_signed_27) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_8) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_3) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_6) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_28) return std_logic_vector;
  function toSLV (p : coreclash_types.tup3) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_tup3_0) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2) return std_logic_vector;
  function toSLV (p : coreclash_types.tup2_0) return std_logic_vector;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (p : coreclash_types.tup2_1) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_array_of_17_signed_27) return std_logic_vector;
  function toSLV (value :  coreclash_types.array_of_tup3_2) return std_logic_vector;
  function toSLV (p : coreclash_types.tup3_1) return std_logic_vector;
end;

package body coreclash_types is
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function toSLV (value :  coreclash_types.array_of_signed_27) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_signed_27(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 27);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 27) + 1 to i*27) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_1) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_std_logic_vector_1(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 1);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 1) + 1 to i*1) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function toSLV (p : coreclash_types.tup3_0) return std_logic_vector is
  begin
    return (toSLV(p.tup3_0_sel0) & toSLV(p.tup3_0_sel1) & toSLV(p.tup3_0_sel2));
  end;
  function toSLV (p : coreclash_types.tup2_2) return std_logic_vector is
  begin
    return (toSLV(p.tup2_2_sel0) & toSLV(p.tup2_2_sel1));
  end;
  function toSLV (value :  coreclash_types.array_of_tup2_2) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_tup2_2(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 597);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 597) + 1 to i*597) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup2_7) return std_logic_vector is
  begin
    return (toSLV(p.tup2_7_sel0) & toSLV(p.tup2_7_sel1));
  end;
  function toSLV (p : coreclash_types.tup2_9) return std_logic_vector is
  begin
    return (toSLV(p.tup2_9_sel0) & toSLV(p.tup2_9_sel1));
  end;
  function toSLV (p : coreclash_types.tup2_4) return std_logic_vector is
  begin
    return (toSLV(p.tup2_4_sel0) & toSLV(p.tup2_4_sel1));
  end;
  function toSLV (value :  coreclash_types.array_of_tup2_4) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_tup2_4(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 110);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 110) + 1 to i*110) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup3_2) return std_logic_vector is
  begin
    return (toSLV(p.tup3_2_sel0) & toSLV(p.tup3_2_sel1) & toSLV(p.tup3_2_sel2));
  end;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_70) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_std_logic_vector_70(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 70);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 70) + 1 to i*70) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (value :  coreclash_types.array_of_unsigned_2) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_unsigned_2(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup2_5) return std_logic_vector is
  begin
    return (toSLV(p.tup2_5_sel0) & toSLV(p.tup2_5_sel1));
  end;
  function toSLV (value :  coreclash_types.array_of_array_of_4_signed_27) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_array_of_4_signed_27(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 108);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 108) + 1 to i*108) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup2_8) return std_logic_vector is
  begin
    return (toSLV(p.tup2_8_sel0) & toSLV(p.tup2_8_sel1));
  end;
  function toSLV (p : coreclash_types.tup2_3) return std_logic_vector is
  begin
    return (toSLV(p.tup2_3_sel0) & toSLV(p.tup2_3_sel1));
  end;
  function toSLV (p : coreclash_types.tup2_6) return std_logic_vector is
  begin
    return (toSLV(p.tup2_6_sel0) & toSLV(p.tup2_6_sel1));
  end;
  function toSLV (value :  coreclash_types.array_of_std_logic_vector_28) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_std_logic_vector_28(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 28);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 28) + 1 to i*28) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup3) return std_logic_vector is
  begin
    return (toSLV(p.tup3_sel0) & toSLV(p.tup3_sel1) & toSLV(p.tup3_sel2));
  end;
  function toSLV (value :  coreclash_types.array_of_tup3_0) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_tup3_0(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 569);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 569) + 1 to i*569) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup2) return std_logic_vector is
  begin
    return (toSLV(p.tup2_sel0) & toSLV(p.tup2_sel1));
  end;
  function toSLV (p : coreclash_types.tup2_0) return std_logic_vector is
  begin
    return (toSLV(p.tup2_0_sel0) & toSLV(p.tup2_0_sel1));
  end;
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (p : coreclash_types.tup2_1) return std_logic_vector is
  begin
    return (toSLV(p.tup2_1_sel0) & toSLV(p.tup2_1_sel1));
  end;
  function toSLV (value :  coreclash_types.array_of_array_of_17_signed_27) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_array_of_17_signed_27(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 459);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 459) + 1 to i*459) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (value :  coreclash_types.array_of_tup3_2) return std_logic_vector is
    alias ivalue    : coreclash_types.array_of_tup3_2(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 81);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 81) + 1 to i*81) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function toSLV (p : coreclash_types.tup3_1) return std_logic_vector is
  begin
    return (toSLV(p.tup3_1_sel0) & toSLV(p.tup3_1_sel1) & toSLV(p.tup3_1_sel2));
  end;
end;
