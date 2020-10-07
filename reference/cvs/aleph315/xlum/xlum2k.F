      LOGICAL FUNCTION XLUM2K(DUMMY)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization, both
C! for SICAL and LCAL with year dependent setup
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!    modified    B.Bloch                   March 2K
C!                clone copy of the previously defined XLUMOK
C!                no check on E_stab bit
C!
C!    Output:     - XLUM2K  /L      both LCAL and SICAL okay, or SICAL
C!                                  not existing
C!
C!    Description
C!    ===========
C!    see routine XLSLUM
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL XLUM,SLUM,LLUM
      CALL XLSLUM(XLUM,SLUM,LLUM)
      XLUM2K = XLUM
      END
#endif
