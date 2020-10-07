      SUBROUTINE DERIV(E,EM1,EM2,C,DE1,DE2,DELTACOS,GED)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Auxiliary to KINEFIT
C    Author   :- Marcello Maggi        27-Jan-1992
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-G,O-Z)
      PARAMETER (PIMASS=0.1349739)
      DIMENSION GED(2),E(2)
      E1=E(1)
      E2=E(2)
      ADD1=-1.D0*(E1-EM1)/DE1/DE1
      ADD2=-1.D0*(E2-EM2)/DE2/DE2
      SQUAREM=PIMASS*PIMASS
      ADDC=-1.D0*SQUAREM*(1.D0-C-SQUAREM/2.D0/E1/E2)
     >                    /E1/E2/DELTACOS/DELTACOS/2.D0
      GED(1) = ADD1+ ADDC/E1
      GED(2) = ADD2+ ADDC/E2
      RETURN
      END
