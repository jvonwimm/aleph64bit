      SUBROUTINE ECISTA
C.----------------------------------------------------------------
C  M.Rumpf jan 86
C! Initialise ECAL Statistics
C   -Called by ECIRUN
C.----------------------------------------------------------------
      SAVE
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
C
      DO 1 I=1,NECST
        NECONT(I) = 0.
        ECCONT(I) = 0.
    1 CONTINUE
C
      END
