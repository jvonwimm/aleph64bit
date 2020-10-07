      LOGICAL FUNCTION llumok(dum)
CKEY XLUMOK TRIG /USER
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization for LCAL
C! Called from user
C!    Author:     H. Meinhard/B.Bloch       26-Apr-1993
C!    mofified    B.Bloch                   March 2K
C!                makes it check also the E_stable bit for HE data in 2K
C!
C!    Output:     - LLUMOK  /L      LCAL okay
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
      llumok = llum.and.XESTAB(KRUN)
      RETURN
      END
#endif
