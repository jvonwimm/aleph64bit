      SUBROUTINE UFGAIN(V, C, DET, W, GAIN)
C!  Calculate Kalman gain matrix
#ifndef DOC
      DOUBLE PRECISION V(2,2), C(5,5), DET, W(2,2), GAIN(5,2)

      W(2,2) = V(1,1) + C(1,1)
      W(1,2) =-V(1,2) - C(1,2)
      W(1,1) = V(2,2) + C(2,2)
      DET = ( W(2,2)*W(1,1) - W(1,2)**2 )
      IF (DET.EQ.0.D0) THEN
        CALL DVZERO(W,4)
        CALL DVZERO(GAIN,10)
        RETURN
      ENDIF
      W(1,1) = W(1,1) / DET
      W(2,2) = W(2,2) / DET
      W(1,2) = W(1,2) / DET
      W(2,1) = W(1,2)
      DET = W(1,1) * W(2,2) - W(1,2)**2
      DO J = 1, 2
        DO I = 1, 5
          GAIN(I,J) = C(I,1)*W(1,J) + C(I,2)*W(2,J)
        ENDDO
      ENDDO

      RETURN
      END
#endif
