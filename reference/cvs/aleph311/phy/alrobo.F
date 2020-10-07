      SUBROUTINE ALROBO(THE,PHI,BEX,BEY,BEZ,ITYP)
C==================================================================
C!    ALEPHLIB copy of the LUND routine LUROBO .
CKEY PHYSICS LUND THRUST E-FLOW / USER
C     Rotate vector P(i,5) from comdeck LUNDCOM
C           THE : Teta angle of rotation
C           PHI : Phi angle of rotation
C           BEX,BEY,BEZ : Centre of rotation coordinate
C           ITYP        : type of fit (1=Calorimeters, 2=Masks)
C     called by subroutine Althru
C     Adapted by M.N.Minard & M.Pepe
C
C==================================================================
#ifndef DOC
      SAVE
#include "lundcom.h"
#include "lundequ.h"
      DIMENSION ROT(3,3),PV(3)
      DOUBLE PRECISION DP(4),DBEX,DBEY,DBEZ,DGA,DBEP,DGABEP
      IF(THE**2+PHI**2.LT.1E-20) GOTO 130
C...ROTATE (TYPICALLY FROM Z AXIS TO DIRECTION THETA,PHI)
      ROT(1,1)=COS(THE)*COS(PHI)
      ROT(1,2)=-SIN(PHI)
      ROT(1,3)=SIN(THE)*COS(PHI)
      ROT(2,1)=COS(THE)*SIN(PHI)
      ROT(2,2)=COS(PHI)
      ROT(2,3)=SIN(THE)*SIN(PHI)
      ROT(3,1)=-SIN(THE)
      ROT(3,2)=0.
      ROT(3,3)=COS(THE)
      DO 120 I=  1 , N
         IF(K(I,1).NE.ITYP) GOTO 120
         DO 100 J=1,3
  100    PV(J)=P(I,J)
         DO 110 J=1,3
  110    P(I,J)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)+ROT(J,3)*PV(3)
  120 CONTINUE
  130 IF(BEX**2+BEY**2+BEZ**2.LT.1E-20) RETURN
C...LORENTZ BOOST (TYPICALLY FROM REST TO MOMENTUM/ENERGY=BETA)
      DBEX=DBLE(BEX)
      DBEY=DBLE(BEY)
      DBEZ=DBLE(BEZ)
      DGA=1D0/DSQRT(1D0-DBEX**2-DBEY**2-DBEZ**2)
      DO 150 I= 1 , N
         IF(K(I,1).NE.ITYP) GOTO 150
         DO 140 J=1,4
  140    DP(J)=DBLE(P(I,J))
         DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
         DGABEP=DGA*(DGA*DBEP/(1D0+DGA)+DP(4))
         P(I,1)=SNGL(DP(1)+DGABEP*DBEX)
         P(I,2)=SNGL(DP(2)+DGABEP*DBEY)
         P(I,3)=SNGL(DP(3)+DGABEP*DBEZ)
         P(I,4)=SNGL(DGA*(DP(4)+DBEP))
  150 CONTINUE
      RETURN
      END
#endif
