C
C  2-d fit
C
      REAL FUNCTION DMINF2(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2,P3
      REAL*8 RESOL
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITP(3))**2/2.)/FITP(3)
      P2(XXXX) = EXP(-XXXX/FITP(4))/FITP(4)
      P3(XXXX) = EXP(-XXXX/FITP(5))/FITP(5)
      RESOL(XXXX) = (SUM-FITP(1)-FITP(2))*P1(XXXX)+
     &  FITP(1)*P2(XXXX)+FITP(2)*P3(XXXX)
C
      XXXX = X
      DMINF2 = RESOL(XXXX)
      RETURN
      END
