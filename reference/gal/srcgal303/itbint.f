      SUBROUTINE ITBINT(TRIGT,IBIN)
C.
C...ITBINT  1.00  860609  19:55                        R.Beuselinck
C.
C!  Calculate the theta bin number corresponding to the input trigger
C.  time for the R-phi-Z trigger.
C.
C.  Calling arguments:
C.  TRIGT - Time of valid trigger coincidence (ns)           (INPUT)
C.  IBIN  - Bin number resulting in range 1 to 256          (OUTPUT)
C.
C-----------------------------------------------------------------------
      SAVE
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
C
      IBIN = (TRIGT - TSTRIT)/TBINIT
      IF (IBIN.LT.1 .OR. IBIN.GT.256) IBIN = 0
      END
