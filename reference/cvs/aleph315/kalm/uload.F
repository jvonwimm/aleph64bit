      SUBROUTINE ULOAD(DBL_TRK, C_NEW, TRK_OUT, COV_OUT)
C!  Convert state vector and covariance matrix to REAL*4
#ifndef DOC
      DOUBLE PRECISION DBL_TRK(*), C_NEW(*)
      REAL TRK_OUT(*), COV_OUT(*)

      TRK_OUT(1) = SNGL(DBL_TRK(1))
      TRK_OUT(2) = SNGL(DBL_TRK(2))
      TRK_OUT(3) = SNGL(DBL_TRK(3))
      TRK_OUT(4) = SNGL(DBL_TRK(4))
      TRK_OUT(5) = SNGL(DBL_TRK(5))

      DO I = 1, 21
        COV_OUT(I) = SNGL(C_NEW(I))
      ENDDO

      RETURN
      END

#endif
