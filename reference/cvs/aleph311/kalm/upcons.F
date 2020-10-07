      SUBROUTINE UPCONS(FACT,C)
C! Apply momentum constraint to filtered covariance matrix
#ifndef DOC
      DOUBLE PRECISION FACT,C(5,5)

      C(5,5) = FACT * FACT * C(4,4)
      C(1,5) = FACT * C(1,4)
      C(5,1) = C(1,5)
      C(2,5) = FACT * C(1,4)
      C(5,2) = C(1,5)
      C(3,5) = FACT * C(1,4)
      C(5,3) = C(1,5)
      C(4,5) = FACT * C(1,4)
      C(5,4) = C(1,5)

      RETURN
      END
#endif
