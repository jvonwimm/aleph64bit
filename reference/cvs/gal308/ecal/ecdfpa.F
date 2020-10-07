*DK ecdfpa
      SUBROUTINE ECDFPA
C.----------------------------------------------------------------
C  M.Rumpf dec 85
C! Define ECAL parameters
C  - Called by ECIRUN
C  - Calls EHPASR,EDINIP
C.----------------------------------------------------------------
#ifndef DOC
C
C  Fast tracking parameters
C
      CALL EHCUTP
C
C
C  Shower generation parameters
C
      CALL EHPASR
C
C  Digitization parameters
C
      CALL EDINIP
C
      END
#endif
