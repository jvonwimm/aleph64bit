      SUBROUTINE CLAS26 (IGOOD)
C----------------------------------------------------------------------
CKEY EDIR CLASS26
C!  - Select CLAS 26 events (alignement/claibration purpose events)
C!
C!   Author   :- E. Lancon              2-APR-1993
C!
C!   Inputs:    None
C!        -
C!
C!   Outputs:   IGOOD     Logical Flag
C!        -
C!
C!   Description  Call EEWIRS and MUTRGS
C!   ===========  To select muon and bhabha events
C!
C?
C!======================================================================
#ifndef DOC
      LOGICAL IGOOD
      LOGICAL EEWIRS, MUTRGS
C --
      IGOOD = EEWIRS(IDUM) .OR. MUTRGS(IDUM)
C --
  999 RETURN
      END
#endif
