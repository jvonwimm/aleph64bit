      LOGICAL FUNCTION INTOF4(ISTORE,KF4GRI)
C---------------------------------------------------------------
C   Marc Verderi                                       2-10-94
C! true if the storey istore belongs to the the array kf4gri
C---------------------------------------------------------------
#ifndef DOC
      INTEGER  ISTORE,KF4GRI(2,2,3)
      INTEGER  I,J,K

      INTOF4 = .FALSE.
      DO I = 1,2
         DO J = 1,2
            DO K = 1,3
               IF (ISTORE.EQ.KF4GRI(I,J,K)) THEN
                  INTOF4 = .TRUE.
                  RETURN
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
#endif
