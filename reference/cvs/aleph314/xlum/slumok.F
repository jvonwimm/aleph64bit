      LOGICAL FUNCTION slumok(dum)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization for SICAL 
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!
C!    Output:     - SLUMOK  /L      SICAL okay, or SICAL not existing
C! 
C!
C!    Description
C!    ===========
C!    see routine xlslum
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL xlum,llum,slum
      CALL xlslum(xlum,slum,llum)
      slumok = slum
      RETURN
      END
#endif
