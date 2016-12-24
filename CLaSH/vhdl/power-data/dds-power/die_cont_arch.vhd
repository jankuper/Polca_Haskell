-------------------------------------------------------------------------------
-- File         : die_cont_arch.vhd
-- Description  : entity for electronic die
-- Author       : Bert Molenkamp, University of Twente
--                with modifications of Sabih Gerez, University of Twente
-- Creation date: August 17, 2009 (header added)
-------------------------------------------------------------------------------
-- $Rev: 57 $
-- $Author: gerezsh $
-- $Date: 2009-09-11 13:47:44 +0200 (Fri, 11 Sep 2009) $
-- $Log$
-------------------------------------------------------------------------------

-- The die in the "cont" architecture is based on the priniple of
-- CONTinuously writing numbers corresponding to the value of an internal
-- conunter to the display; when the button is pressed, this writing is
-- interrupted and a stable value can be read on the 7-segment display.

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

architecture cont of die is
  signal die_value : integer range 0 to 5;
begin
  process(reset,clk)
  begin
    if reset='1' then
      die_value <= 1;
      display <= "1111110"; -- display a 0 at reset
    elsif rising_edge(clk) then
      if die_value<5 then
        die_value <= die_value + 1;
      else 
        die_value <= 0;
      end if;
      if button='0' then  
        case die_value is    --0123456
          when 0 => display <= "0110000"; -- display a 1
          when 1 => display <= "1101101"; -- display a 2
          when 2 => display <= "1111001"; -- display a 3
          when 3 => display <= "0110011"; -- display a 4
          when 4 => display <= "1011011"; -- display a 5
          when 5 => display <= "1011111"; -- display a 6
        end case;      
      end if;
    end if;
  end process;
end cont;
