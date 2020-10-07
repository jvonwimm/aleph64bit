      SUBROUTINE UFMMUL(RES, A, B, MATDIM)
C! res = a * b, where res, a, b, are 5x5 double precision matrices
C! matdim specifies the dimension of the matrix to multiply.
#ifndef DOC
      IMPLICIT NONE
C
      INTEGER MATDIM
      DOUBLE PRECISION RES(5,5),A(5,5),B(5,5),VEC(5),SCALE,C(5,5)
C
      INTEGER I, J, K, NDIM
      DOUBLE PRECISION TMP, WORK(5), E(5,5)
C
      NDIM = MATDIM
        DO I = 1, NDIM
          DO J = 1, NDIM
            TMP = 0.0D0
            DO K = 1, NDIM
              TMP = TMP + A(J,K) * B(K,I)
            ENDDO
            RES(J,I) = TMP
          ENDDO
        ENDDO
C
      RETURN
C
      ENTRY UFMMLT(RES, A, B, MATDIM)
C! res = a * b^T, where res, a, b, are 5x5 double precision matrices
C! matdim specifies the dimension of the matrix to multiply.
C
      NDIM = MATDIM
        DO I = 1, NDIM
          DO J = 1, NDIM
            TMP = 0.0D0
            DO K = 1, NDIM
              TMP = TMP + A(J,K) * B(I,K)
            ENDDO
            RES(J,I) = TMP
          ENDDO
        ENDDO
C
      RETURN
C
      ENTRY  UFMSCL(RES, A, VEC, MATDIM)
C! res(i,j) = a(i,j) / (vec(i)*vec(j)), where res and a are 5x5
C!     double precision matrices and vec is dimension 5 double precision
C!     vector.
C! matdim specifies the dimension of the matrix to multiply.
C
      NDIM = MATDIM
        DO I = 1, NDIM
          WORK(I) = 1.0D0 / VEC(I)
        END DO
        DO I = 1, NDIM
          TMP = WORK(I)
          DO J = 1, NDIM
            RES(J,I) = A(J,I) * TMP * WORK(J)
          END DO
        END DO
C
      RETURN
C
      ENTRY UFMADD(A,SCALE,B,RES)
C! Add two 5x5 matrices, with scaling: RES = A + SCALE * B
C
      DO I = 1, 5
        DO J = 1, 5
          RES(J,I) = A(J,I) + SCALE * B(J,I)
        ENDDO
      ENDDO
C
      RETURN
C
      ENTRY UFMULT(A,B,C,RES)
C! Multiply three 5x5 matrices, with transpose (RES = A * B * C^T)

      DO I = 1, 5
        DO J = 1, 5
          TMP = 0.D0
          DO K = 1, 5
            TMP = TMP + B(I,K)*C(J,K)
          ENDDO
          E(I,J) = TMP
        ENDDO
      ENDDO

      DO I= 1, 5
        DO J = 1, I
          TMP = 0.D0
          DO K = 1, 5
            TMP = TMP + A(I,K)*E(K,J)
          ENDDO
          RES(I,J) = TMP
          RES(J,I) = TMP
        ENDDO
      ENDDO
C
      RETURN
C
      END
#endif
