      LOGICAL FUNCTION LKEQUAL(STR1,STR2)
C -----------------------------------------------------------
C ------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) STR1,STR2
      INTEGER POS
C -----------------------------------------------------------
      LEN=LKLWRD(STR1)
      L2=INDEX(STR1,'=')
      IF (L2.GT.0.AND.L2.LE.LEN) LEN=L2-1
      IF (LEN.GT.-1) THEN
          IF (STR1(1:LEN).EQ.STR2(1:LEN)) THEN
              LKEQUAL=.TRUE.
          ELSE
              LKEQUAL=.FALSE.
          ENDIF
      ELSE
          LKEQUAL=.FALSE.
      ENDIF
      RETURN
      END
#endif
