-------------------------------------------------------------------------------
-- File         : tb_die.vhd
-- Description  : Testbench for die
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

entity tb_die is 
end tb_die;

architecture structure of tb_die is
  -- declare components to be instantiated
  component die is
    port (clk     : in std_logic;
          reset   : in std_logic;
          button  : in std_logic;
          display : out std_logic_vector(0 to 6)); 
  end component;

  component tvc_die is
    port (clk     : out std_logic;
          reset   : out std_logic;
          button  : out std_logic;
          display : in  std_logic_vector(0 to 6)); 
  end component;

  -- declare local signals
  signal clk, reset, button: std_logic;
  signal display: std_logic_vector(0 to 6);
begin
  -- instantiate and interconnect components
  -- note that the generic word_length is passed to the subblocks
  duv: die
    port map (clk => clk, reset => reset, 
              button => button, display => display);
  tvc: tvc_die
    port map (clk => clk, reset => reset, 
              button => button, display => display);
end structure;
