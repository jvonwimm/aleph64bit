      SUBROUTINE CHISCOM(E,EM1,EM2,C,DE1,DE2,DELTACOS,CHI2)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Auxiliary to KINEFIT
C    Author   :- Marcello Maggi        27-Jan-1992
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-G,O-Z)
      PARAMETER (PIMASS=0.1349739D0)
      DIMENSION E(2)
      SQUAREM=PIMASS*PIMASS
      CHI_1 = (E(1)-EM1)*(E(1)-EM1)/DE1/DE1
      CHI_2 = (E(2)-EM2)*(E(2)-EM2)/DE2/DE2
      CHI_3 = (1.D0-C-SQUAREM/2.D0/E(1)/E(2))*
     >        (1.D0-C-SQUAREM/2.D0/E(1)/E(2))/DELTACOS/DELTACOS
      CHI2 = CHI_1 +CHI_2 + CHI_3
      RETURN
      END
