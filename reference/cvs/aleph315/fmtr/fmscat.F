      SUBROUTINE FMSCAT(PINPUT,STEPM,XMASS,RADL,POUT)
C---------------------------------------------------------------------
C!  Compute displacent of position and direction of track over step
C!  of length STEP adopting the Gaussian formalism used in routine
C!  GMUL in GEANT 3.10. (see GEANT3 manual, PHYS 320).
C---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "fmgeom.h"
#include "alcons.h"
C
      DIMENSION PINPUT(7),POUT(7),PDIR(3),DIN(3)
      DIMENSION ROTMAT(3,3)
      EQUIVALENCE (PDIR(1),ROTMAT(1,3))
C
      DATA SQRT3I/0.57735/
C
      CALL UCOPY(PINPUT(4),PDIR(1),3)
      PMOM=PINPUT(7)
      GETOT = SQRT(PMOM*PMOM + XMASS*XMASS)
C
C             Computes mean deviation angles in radian
C
      STR=ABS(STEPM/RADL)
      SIGMA  = 0.015*GETOT*SQRT(STR)/(PMOM*PMOM)
      IF(SIGMA.GT.10.)SIGMA=10.
C
   5  R1     = RNDM(1)
      RX1    = 100.*R1
      IX1    = RX1 + 1
      IF(IX1.GT.2.AND.IX1.LT.99)THEN
         XRAT= IX1-RX1
         X1  = (1.-XRAT)*SQRMUL(IX1+1) +XRAT*SQRMUL(IX1)
      ELSE
         X1  = SQRT(-2.*ALOG(R1))
         IF(X1.GT.10.)GO TO 5
      ENDIF
      IY2    = 100.*RNDM(2) + 1
      SY1    = SINMUL(IY2)
      CY1    = COSMUL(IY2)
      AA     = X1*SY1
      BB     = X1*CY1
      THETA  = X1*SIGMA
      IF(THETA.LT.0.05)THEN
         DIN(1) = THETA*SY1
         DIN(2) = THETA*CY1
         DIN(3) = 1.-0.5*THETA*THETA
      ELSE
         SINT   = SIN(THETA)
         DIN(1) = SINT*SY1
         DIN(2) = SINT*CY1
         DIN(3) = COS(THETA)
      ENDIF
C
C  Computes rotation matrix around particle direction
C
      VMM    = SQRT(PDIR(1)*PDIR(1) + PDIR(2)*PDIR(2))
      IF (VMM.NE.0.) THEN
C
         ROTMAT(1,2) = - PDIR(2)/VMM
         ROTMAT(2,2) = PDIR(1)/VMM
         ROTMAT(3,2) = 0.
C
         ROTMAT(1,1) = ROTMAT(2,2)*PDIR(3)
         ROTMAT(2,1) = - ROTMAT(1,2)*PDIR(3)
         ROTMAT(3,1) = - VMM
      ELSE
C
         ROTMAT(1,1) = 1.
         ROTMAT(2,1) = 0.
         ROTMAT(3,1) = 0.
         ROTMAT(1,2) = 0.
         ROTMAT(2,2) = 1.
         ROTMAT(3,2) = 0.
C
      ENDIF
C
C Compute deviation of positions
C
      SIGMA2 = 0.5*SIGMA*STEPM
  35  R3     = RNDM(3)
      RX3    = 100.*R3
      IX3    = RX3 + 1
      IF(IX3.GT.2.AND.IX3.LT.99)THEN
         XRA3= IX3-RX3
         X3  = (1.-XRA3)*SQRMUL(IX3+1) +XRA3*SQRMUL(IX3)
      ELSE
         X3  = SQRT(-2.*ALOG(R3))
         IF(X3.GT.10.)GO TO 35
      ENDIF
      IY4    = 100.*RNDM(4) + 1
      CC     = X3*SINMUL(IY4)
      DD     = X3*COSMUL(IY4)
      DX     = (AA+CC*SQRT3I)*SIGMA2
      DY     = (BB+DD*SQRT3I)*SIGMA2
C
C  Correct step size
C
C      STEPM=SQRT(STEPM*STEPM+DX*DX+DY*DY)
C
C  Compute new space point and direction cosinus
C
  40  CONTINUE
      DO 50 I=1,3
         POUT(I+3)=ROTMAT(I,1)*DIN(1)+ROTMAT(I,2)*DIN(2)
     +            +ROTMAT(I,3)*DIN(3)
         POUT(I)  =PINPUT(I)+ROTMAT(I,1)*DX+ROTMAT(I,2)*DY
  50  CONTINUE
C
      RETURN
      END
#endif
