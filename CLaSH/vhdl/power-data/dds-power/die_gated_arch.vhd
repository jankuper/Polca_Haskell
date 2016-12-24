-------------------------------------------------------------------------------
-- File         : die_gated_arch.vhd
-- Description  : entity for electronic die
-- Author       : Bert Molenkamp, University of Twente
--                with modifications of Sabih Gerez, University of Twente
-- Creation date: August 23, 2009 
-------------------------------------------------------------------------------
-- $Rev: 57 $
-- $Author: gerezsh $
-- $Date: 2009-09-11 13:47:44 +0200 (Fri, 11 Sep 2009) $
-- $Log$
-------------------------------------------------------------------------------

-- The die in the "gated" architecture is derived from the "cont"
-- architecture. However, instead of continuously copying the internal
-- counter value to the display as long as the button is not pressed, the
-- value is only written to the display when the button changes position
-- from depressed to pressed (the rising edge of the button signal).

-- 7-segment display coding
-- 
--     (0)
--    +---+
--    |   |
-- (5)|   |(1)
--    |(6)|
--    +---+
--    |   |
-- (4)|   |(2)
--    |(3)|
--    +---+

architecture gated of die is
  signal button_prev: std_logic;
  signal die_value: integer range 0 to 5;
begin
  process(reset,clk)
  begin
    if reset='1' then
      die_value <= 1;
      button_prev <= '0';
      display <= "1111110"; -- display a 0 at reset
    elsif rising_edge(clk) then
      if die_value < 5 then
        die_value <= die_value + 1;
      else 
        die_value <= 0;
      end if;

      -- remember button value for next iteration
      button_prev <= button;

      -- only update display on rising edge of "button"
      if button='1' and button_prev ='0' then  
        case die_value is    --0123456
          when 0 => display <= "0110000";
          when 1 => display <= "1101101";
          when 2 => display <= "1111001";
          when 3 => display <= "0110011";
          when 4 => display <= "1011011";
          when 5 => display <= "1011111";
        end case;      
      end if;
    end if;
  end process;
end gated;
