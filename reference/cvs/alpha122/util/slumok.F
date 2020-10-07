      LOGICAL FUNCTION SLUMOK(DUMMY)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization, for SICAL
C! Called from user
C!    Author:     H. Meinhard       26-Apr-1993
C!
C!    Output:     - SLUMOK  /L      SICAL okay
C!
C!    Description
C!    ===========
C!    see routine XLSLUM
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL XLUM,SLUM,LLUM
      CALL XLSLUM(XLUM,SLUM,LLUM)
      SLUMOK = SLUM
      END
#endif
