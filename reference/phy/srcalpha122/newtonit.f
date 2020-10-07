      SUBROUTINE NEWTONIT(EFIT,GED,SHES,EFIN)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Auxiliary to KINEFIT
C    Author   :- Marcello Maggi        27-Jan-1992
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-G,O-Z)
      DIMENSION SHES(2,2),GED(2),EFIT(2),EFIN(2)
      DO 10 I=1,2
       EFIN(I)=EFIT(I)+SHES(I,1)*GED(1)+SHES(I,2)*GED(2)
       IF(EFIN(I).LE.0.2D0) EFIN(I)=0.2D0
   10 CONTINUE
      RETURN
      END
