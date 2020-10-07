      SUBROUTINE UFCOVA(C_PRED,GAIN,C_FILT)
C! Calculate filtered covariance matrix
#ifndef DOC
      DOUBLE PRECISION C_PRED(5,5),GAIN(5,2),C_FILT(5,5)
      DOUBLE PRECISION TMP1, TMP2

      DO J = 1, 5
        TMP1 = C_PRED(1,J)
        TMP2 = C_PRED(2,J)
        DO  I = 1, 5
          C_FILT(I,J) = C_PRED(I,J) - GAIN(I,1)*TMP1 - GAIN(I,2)*TMP2
        ENDDO
      ENDDO

      RETURN
      END
#endif
