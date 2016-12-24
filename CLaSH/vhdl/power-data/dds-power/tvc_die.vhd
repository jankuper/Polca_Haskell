-------------------------------------------------------------------------------
-- File         : tvc_die.vhd
-- Description  : test-vector controller for die entity
-- Author       : Sabih Gerez, University of Twente
-- Creation date: August 17, 2009
-------------------------------------------------------------------------------
-- $Rev: 56 $
-- $Author: gerezsh $
-- $Date: 2009-09-08 16:59:53 +0200 (Tue, 08 Sep 2009) $
-- $Log$
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity tvc_die is
  port (clk     : out std_logic;
        reset   : out std_logic;
        button  : out std_logic;
        display : in  std_logic_vector(0 to 6)); 
end tvc_die;


architecture rare_push of tvc_die is
  -- constant half_clock_period: time := 100 ms;
  constant half_clock_period: time := 10 ns;
  signal thrown_value: integer;
  -- internal clock and reset signals (these signals are necessary
  -- because VHDL does not allow that output signals are read in the
  -- entity that generates them)

begin

  -- generate clock
  clock: process
  begin
    clk <= '1';
    wait for half_clock_period;
    clk <= '0';
    wait for half_clock_period;
  end process clock;

  gen_reset: process
  begin
    reset <= '1';
    wait for 3*half_clock_period;
    reset <= '0';
    wait;
  end process gen_reset;
  
  gen_button: process
    -- constant button_down_period:time := 543 ms;
    -- constant button_up_period:time := 9876 ms;
    constant button_down_period1: time := 9876 ns;
    constant button_up_period1:   time :=  587 ns;
    constant button_down_period2: time := 8901 ns;
    constant button_up_period2:   time :=  543 ns;
  begin
    button <= '0';
    wait for button_down_period1;
    button <= '1';
    wait for button_up_period1;
    button <= '0';
    wait for button_down_period2;
    button <= '1';
    wait for button_up_period2;
  end process gen_button;

  seven_segment: process (display)
  begin
    case display is
      when "1111110" => thrown_value <= 0;
      when "0110000" => thrown_value <= 1;
      when "1101101" => thrown_value <= 2;
      when "1111001" => thrown_value <= 3;
      when "0110011" => thrown_value <= 4;
      when "1011011" => thrown_value <= 5;
      when "1011111" => thrown_value <= 6;
      when others    => thrown_value <= 7; -- cannot occur in this design
    end case;
  end process seven_segment;
end rare_push;
