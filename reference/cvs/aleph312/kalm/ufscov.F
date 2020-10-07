      SUBROUTINE UFSCOV(C_SMTHKP1, C_PREDKP1, C_FILT,
     $       GAIN_SMOOTH, C_SMOOTH, MATDIM)
C! Compute the smoothed covariance matrix
C! C_SMOOTH = C_FILT + GAIN * (C_SMOOTH(k+1) - C_PRED(k+1)) * GAIN^T
#ifndef DOC

      IMPLICIT NONE

      DOUBLE PRECISION C_SMTHKP1(5,5), C_PREDKP1(5,5), C_FILT(5,5)
      DOUBLE PRECISION GAIN_SMOOTH(5,5), C_SMOOTH(5,5)
      INTEGER MATDIM

      DOUBLE PRECISION WORK(5,5), E(5,5), SUM
      INTEGER I, J, K

      IF (MATDIM .EQ. 5) THEN
        DO  I = 1, 5
          DO  J = 1, 5
            WORK(J,I) = C_SMTHKP1(J,I) - C_PREDKP1(J,I)
          END DO
        END DO

        DO I = 1, 5
          DO J = 1, 5
            SUM = 0.0D0
            DO K = 1, 5
              SUM = SUM + WORK(I,K)*GAIN_SMOOTH(J,K)
            ENDDO
            E(I,J) = SUM
          ENDDO
        ENDDO

        DO I= 1, 5
          DO J = 1, I
            SUM = 0.D0
            DO K = 1, 5
              SUM = SUM + GAIN_SMOOTH(I,K)*E(K,J)
            ENDDO
            C_SMOOTH(I,J) = SUM + C_FILT(I,J)
            C_SMOOTH(J,I) = C_SMOOTH(I,J)
          ENDDO
        ENDDO
      ELSE
        DO  I = 1, 4
          DO  J = 1, 4
            WORK(J,I) = C_SMTHKP1(J,I) - C_PREDKP1(J,I)
          END DO
        END DO

        DO I = 1, 4
          DO J = 1, 4
            SUM = 0.0D0
            DO K = 1, 4
              SUM = SUM + WORK(I,K)*GAIN_SMOOTH(J,K)
            ENDDO
            E(I,J) = SUM
          ENDDO
        ENDDO

        DO I= 1, 4
          DO J = 1, I
            SUM = 0.D0
            DO K = 1, 4
              SUM = SUM + GAIN_SMOOTH(I,K)*E(K,J)
            ENDDO
            C_SMOOTH(I,J) = SUM + C_FILT(I,J)
            C_SMOOTH(J,I) = C_SMOOTH(I,J)
          ENDDO
        ENDDO
      END IF

      RETURN
      END
#endif
