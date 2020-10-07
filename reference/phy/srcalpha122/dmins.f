C
C  Low-statistics fit function
C
      REAL FUNCTION DMINS(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2
      REAL*8 RESOL
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = XXXX*EXP(-(XXXX/FITP(2))**2/2.)/FITP(2)**2
      P2(XXXX) = XXXX*EXP(-XXXX/FITP(3))/FITP(3)**2
      RESOL(XXXX) = (SUM-FITP(1))*P1(XXXX)+FITP(1)*P2(XXXX)
C
      XXXX = X
      DMINS = RESOL(XXXX)
      RETURN
      END
