      SUBROUTINE SMNORM(V,N)
C ---------------------------------------------------------------
C!    NORMALISE SYMMETRIC N*N MATRIX SO THAT DIAGONAL ELEMENTS=1
C     AUTHOR: MARTIN POPPE
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION V(*)
      IND(K) = (K+K*K)/2
      IF(N.LT.2)                                    RETURN
      DO 30 NROW = 2,N
        IROW   = IND(NROW)
        IFIRST = IND(NROW-1)+1
        ILAST  = IND(NROW)-1
        DO 20 IOFFD =IFIRST,ILAST
          NCOL   = IOFFD-IFIRST+1
          ICOL   = IND(NCOL)
          V(IOFFD) = V(IOFFD)/SQRT(V(ICOL)*V(IROW))
  20    CONTINUE
  30  CONTINUE
      DO 40  NROW =1,N
  40    V(IND(NROW))=1.0
      RETURN
      END
#endif
