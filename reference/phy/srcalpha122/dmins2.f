C
C  Low-statistics 2-d fit function
C
      REAL FUNCTION DMINS2(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2
      REAL*8 RESOL
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITP(2))**2/2.)/FITP(2)
      P2(XXXX) = EXP(-XXXX/FITP(3))/FITP(3)
      RESOL(XXXX) = (SUM-FITP(1))*P1(XXXX)+FITP(1)*P2(XXXX)
C
      XXXX = X
      DMINS2 = RESOL(XXXX)
      RETURN
      END
