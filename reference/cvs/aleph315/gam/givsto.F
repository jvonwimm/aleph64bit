      SUBROUTINE GIVSTO(ESTO1,ESTO2,ESTO3,INDSTO,ISTORE,ESTO,KSTACK)
C-------------------------------------------------------------------
C     M. Verderi                                             2-10-94
C! Reads in the general arrays ESTOi the individual informations of
C the storey istore (PEST number). (avoid loops on ESTOi)
C
C Called by GAMPEX
C-------------------------------------------------------------------
#ifndef DOC
      INTEGER    ISTORE,KSTACK,ISTFND,I
      REAL       ESTO1(9,*)
      REAL       ESTO2(9,*)
      REAL       ESTO3(9,*)
      INTEGER    INDSTO(2,*)
      REAL       ESTO (9)
C------------------------------------------------------
      IF ( ISTORE.GT.0) THEN
      KSTACK = INDSTO(1,ISTORE)
      ISTFND = INDSTO(2,ISTORE)
      IF (KSTACK.EQ.1) THEN
         DO I=1,9
            ESTO(I) = ESTO1(I,ISTFND)
         ENDDO
      ELSEIF (KSTACK.EQ.2) THEN
         DO I=1,9
            ESTO(I) = ESTO2(I,ISTFND)
         ENDDO
      ELSE
         DO I=1,9
            ESTO(I) = ESTO3(I,ISTFND)
         ENDDO
      ENDIF
      ENDIF
      RETURN
      END
#endif
