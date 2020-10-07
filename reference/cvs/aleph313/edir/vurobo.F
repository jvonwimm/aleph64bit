      SUBROUTINE VUROBO(N,P,IFLAG,THE,PHI,BEX,BEY,BEZ)
C-----------------------------------------------------------------------
CKEY EDIR ROTATE/BOOST
C! Routine not documented
C-
C   Input  :
C   Output :
C-
C   Called by   : ULTHRU
C   Calls  : None
C   Input banks : None
C-
C                                    Author: J.C.Brient - 910400
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      DIMENSION P(4,N) ,IFLAG(N)
      DIMENSION ROT(3,3),PV(3)
      DOUBLE PRECISION DP(4),DBEX,DBEY,DBEZ,DGA,DBEP,DGABEP
      IMAX=N
      IF(THE**2+PHI**2.GT.1E-20) THEN
C --
C   Rotate (typically from z axis to direction theta,phi)
C --
        ROT(1,1)=COS(THE)*COS(PHI)
        ROT(1,2)=-SIN(PHI)
        ROT(1,3)=SIN(THE)*COS(PHI)
        ROT(2,1)=COS(THE)*SIN(PHI)
        ROT(2,2)=COS(PHI)
        ROT(2,3)=SIN(THE)*SIN(PHI)
        ROT(3,1)=-SIN(THE)
        ROT(3,2)=0.
        ROT(3,3)=COS(THE)
        DO 120 I=1,IMAX
          IF(IFLAG(I).NE.0) GOTO 120
          DO 100 J=1,3
  100     PV(J)=P(J,I)
           DO 110 J=1,3
  110     P(J,I)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)+
     $           ROT(J,3)*PV(3)
  120   CONTINUE
      ENDIF
      IF(BEX**2+BEY**2+BEZ**2.GT.1E-20) THEN
C --
C   Lorentz boost (typically from rest to momentum/energy = beta)
C --
        DBEX=BEX
        DBEY=BEY
        DBEZ=BEZ
        DGA=1D0/DSQRT(1D0-DBEX**2-DBEY**2-
     $  DBEZ**2)
        DO 140 I=1,IMAX
          IF(IFLAG(I).NE.0) GOTO 140
          DO 130 J=1,4
  130     DP(J)=P(I,J)
          DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
          DGABEP=DGA*(DGA*DBEP/(1D0+DGA)+DP(4))
          P(1,I)=DP(1)+DGABEP*DBEX
          P(2,I)=DP(2)+DGABEP*DBEY
          P(3,I)=DP(3)+DGABEP*DBEZ
          P(4,I)=DGA*(DP(4)+DBEP)
  140   CONTINUE
      ENDIF
      RETURN
      END
#endif
