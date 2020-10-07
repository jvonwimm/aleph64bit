      LOGICAL FUNCTION XLUMOK(DUMMY)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization, both
C! for SICAL and LCAL with year dependent setup
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!    mofified    B.Bloch                   March 2K
C!                makes it check also the E_stable bit for HE data in 2K
C!
C!    Output:     - XLUMOK  /L      both LCAL and SICAL okay, or SICAL
C!                                  not existing
C!
C!    Description
C!    ===========
C!    see routine XLSLUM
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL XLUM,SLUM,LLUM,XESTAB
      EXTERNAL XESTAB
C
      CALL XLSLUM(XLUM,SLUM,LLUM)
C get run and event number
      CALL abruev(irun,ievt)
      krun = irun
C check also on XESTAB
      XLUMOK = XLUM.and.XESTAB(KRUN)
      END
#endif
