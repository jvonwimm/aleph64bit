      SUBROUTINE E4RES3(A,B,C,D,X,Y,Z,IERROR)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : GIVE INTERSECTION OF PLAN WITH EXTRAPOLATED TRACK
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
      DIMENSION A(3),B(3),C(3),D(3),E(3)
      EXTERNAL ECDETE
      ERROR=0
      X=0
      Y=0
      Z=0
      DET=ECDETE(A,B,C)
      IF(DET.EQ.0)THEN
        ERROR=1
        RETURN
      ENDIF
      DO 1 I=1,3
 1    E(I)=-D(I)
      X=ECDETE(E,B,C)/DET
      Y=ECDETE(A,E,C)/DET
      Z=ECDETE(A,B,E)/DET
      RETURN
      END
#endif
