      INTEGER FUNCTION ALTRHV (IBITHV)
C --------------------------------------------------------------
CKEY ALEF TRIGGER HV
C - F.Ranjard -940426
C! Obsolete. Use XHVBIT directly instead - MC 971205
C! returns trigger HV word from XTCN or X1RG
C - Output : - IBITHV  / I = word 4
C              ALTRHV  / I = 0 if no bank there
C                            1 if XTCN is there
C                            2 if X1RG is there
C
C ----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER  IBITHV
      INTEGER  XHVBIT
      EXTERNAL XHVBIT
C ------------------------------------------------------------
      ALTRHV = XHVBIT(IBITHV)
      IF( ALTRHV .GT. 2 ) ALTRHV = 0
C
      END
#endif
