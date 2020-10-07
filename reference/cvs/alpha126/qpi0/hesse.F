      SUBROUTINE HESSE(E,C,DE1,DE2,DELTACOS,SHES,IFAIL)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Auxiliary to KINEFIT
C    Author   :- Marcello Maggi        27-Jan-1992
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT DOUBLE PRECISION (A-G,O-Q,S-Z)
#include "bcs.h"
      PARAMETER (PIMASS=0.1349739D0)
      DIMENSION E(2),SHES(2,2),SINP(2,2),UCHECK(2,2)
      REAL*4 W(2)
      E1=E(1)
      E2=E(2)
      SQUAREM=PIMASS*PIMASS
      ADD=SQUAREM/E1/E2 - (1.D0-C)
      SINP(1,1) = 1.D0/DE1/DE1 +
     >   1.D0*SQUAREM*ADD/E1/E1/E1/E2/DELTACOS/DELTACOS
      SINP(2,2) = 1.D0/DE2/DE2 +
     >   1.D0*SQUAREM*ADD/E1/E2/E2/E2/DELTACOS/DELTACOS
      SINP(1,2) = SQUAREM*ADD/E1/E1/E2/E2/DELTACOS/DELTACOS/2.D0
      SINP(2,1) = SINP(1,2)
      DO 10 I=1,2
      DO 10 J=1,2
        SHES(I,J)=SINP(I,J)
   10 CONTINUE
      CALL DINV(2,SHES,2,W,IFAIL)
      IF(IFAIL.EQ.-1) THEN
        WRITE (IW(6),*)' ## HESSE ## : Matrix SHES is Singular'
        RETURN
      ENDIF
      DO 20 I=1,2
      DO 20 J=1,2
        UCHECK(I,J) = SINP(I,1)*SHES(1,J) + SINP(I,2)*SHES(2,J)
   20 CONTINUE
        IF(UCHECK(1,1).LT. 0.9D0.OR.UCHECK(1,1).GT.1.1D0 .OR.
     >     UCHECK(2,2).LT. 0.9D0.OR.UCHECK(2,2).GT.1.1D0 .OR.
     >     UCHECK(1,2).LT.-0.1D0.OR.UCHECK(1,2).GT.0.1D0 .OR.
     >     UCHECK(2,1).LT.-0.1D0.OR.UCHECK(2,1).GT.0.1D0 ) THEN
          IFAIL=-1
          WRITE (IW(6),*)' ## HESSE ## : Matrix UCHECK not UNIT'
        ENDIF
      RETURN
      END
#endif
