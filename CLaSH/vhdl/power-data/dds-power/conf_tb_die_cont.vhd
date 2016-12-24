-------------------------------------------------------------------------------
-- File         : conf_tb_die_cont.vhd
-- Description  : configuration for tb_die, "cont" architecture
-- Author       : Sabih Gerez, University of Twente
-- Creation date: August 23, 2009
-------------------------------------------------------------------------------
-- $Rev: 56 $
-- $Author: gerezsh $
-- $Date: 2009-09-08 16:59:53 +0200 (Tue, 08 Sep 2009) $
-- $Log$
-------------------------------------------------------------------------------

configuration conf_tb_die_cont of tb_die is
  for structure
    for duv: die use entity work.die(cont);
    end for;
    for tvc: tvc_die use entity work.tvc_die(rare_push);
    end for;
  end for;
end conf_tb_die_cont;
