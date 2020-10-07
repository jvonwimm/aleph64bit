      SUBROUTINE ENORME(A)
C----------------------------------------------------------------------
CKEY ECALDES VECTOR NORMALISATION / INTERNAL
C     H.Videau      Creation 22/12/86   Modification 26/03/86
C! Normalizes the 3-vector A
C  Utility routine
C   Calls: none
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
       REAL A(3),AA

       AA=A(1)**2+A(2)**2+A(3)**2
       IF(AA.LE.0) GO TO 999
       AA=SQRT(AA)
       A(1)=A(1)/AA
       A(2)=A(2)/AA
       A(3)=A(3)/AA
  999  RETURN
       END
#endif
