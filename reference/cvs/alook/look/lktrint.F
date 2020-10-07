      SUBROUTINE LKTRINT(STR,I,C)
C ---------------------------------------------------------
C ---------------------------------------------------------
#ifndef DOC
      CHARACTER STR*25,C
C -----------------------------------------------------------
      I=0
      J=0
      IF (STR(1:1).EQ.'-') THEN
          NEG=-1
          J=J+1
      ELSE
          NEG=1
      ENDIF
 10   J=J+1
      C=STR(J:J)
      IF (C.GE.'0'.AND.C.LE.'9') THEN
          I=I*10+ICHAR(C)-ICHAR('0')
          GOTO 10
      ENDIF
      IF (J.EQ.1.OR.J.EQ.2.AND.NEG.EQ.-1) THEN
          C='A'
      ELSE
          C='I'
          I=I*NEG
      ENDIF
      RETURN
      END
#endif
