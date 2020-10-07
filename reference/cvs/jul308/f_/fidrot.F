      SUBROUTINE FIDROT(TVEC , THE , PHI )
C==================================================================
C! Rotate vector
C           TVEC: Vector to rotate
C           THE : Teta angle of rotation
C           PHI : Phi angle of rotation
C     Author M.N.Minard
C
C?
C!======================================================================
#ifndef DOC
      DIMENSION ROT(3,3),TVEC(3,2), PV (3)
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
      DO 120 I=  1 , 2
         DO 100 J = 1, 3
         PV (J) = TVEC ( J ,I)
 100     CONTINUE
         DO 110 J=1,3
  110    TVEC(J,I)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)
     +  +ROT(J,3)*PV(3)
  120 CONTINUE
 130  CONTINUE
      RETURN
      END
#endif
