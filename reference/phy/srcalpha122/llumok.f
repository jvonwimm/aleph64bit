      LOGICAL FUNCTION LLUMOK(DUMMY)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization, for LCAL
C! Called from user
C!    Author:     H. Meinhard       26-Apr-1993
C!
C!    Output:     - XLUMOK  /L      LCAL okay
C!
C!    Description
C!    ===========
C!    see routine XLSLUM
C---------------------------------------------------------------------
      LOGICAL XLUM,SLUM,LLUM
      CALL XLSLUM(XLUM,SLUM,LLUM)
      LLUMOK = LLUM
      END
