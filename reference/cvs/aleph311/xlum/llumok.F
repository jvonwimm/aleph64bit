      LOGICAL FUNCTION llumok(dum)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization for LCAL
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!
C!    Output:     - LLUMOK  /L      LCAL okay 
C!
C!    Description
C!    ===========
C!    see routine xlslum
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL xlum,llum,slum
      CALL xlslum(xlum,slum,llum)
      llumok = llum
      RETURN
      END
#endif
