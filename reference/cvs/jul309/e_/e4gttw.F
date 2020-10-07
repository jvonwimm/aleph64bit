      SUBROUTINE E4GTTW(I1,I2,J1,J2)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : GET TOWERS (I1 J1) (I1 J2) (I2 J1) (I2 J2)
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
C
C
      KSTAK=1
      CALL E4GTST(I1,I2,J1,J2,KSTAK)
      KSTAK=2
      CALL E4GTST(I1,I2,J1,J2,KSTAK)
      KSTAK=3
      CALL E4GTST(I1,I2,J1,J2,KSTAK)
C
      RETURN
      END
#endif
