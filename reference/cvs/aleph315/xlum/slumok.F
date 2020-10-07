      LOGICAL FUNCTION slumok(dum)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization for SICAL
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!    mofified    B.Bloch                   March 2K
C!               makes it check also the E_stable bit for HE data in 2K
C!
C!    Output:     - SLUMOK  /L      SICAL okay, or SICAL not existing
C!
C!
C!    Description
C!    ===========
C!    see routine xlslum
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL xlum,llum,slum,XESTAB
      EXTERNAL XESTAB
C
      CALL xlslum(xlum,slum,llum)
C get run and event number
      CALL abruev(irun,ievt)
      krun = irun
C check also on XESTAB
      slumok = slum.and.XESTAB(KRUN)
      RETURN
      END
#endif
