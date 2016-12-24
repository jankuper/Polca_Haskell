-------------------------------------------------------------------------------
-- File         : die_ent.vhd
-- Description  : entity for electronic die
-- Author       : Bert Molenkamp, University of Twente
--                with modifications of Sabih Gerez, University of Twente
-- Creation date: August 17, 2009 (header added)
-------------------------------------------------------------------------------
-- $Rev: 56 $
-- $Author: gerezsh $
-- $Date: 2009-09-08 16:59:53 +0200 (Tue, 08 Sep 2009) $
-- $Log$
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity die is
  port (clk     : in std_logic;
        reset   : in std_logic;
        button  : in std_logic;
        display : out std_logic_vector(0 to 6)); 
end die;
