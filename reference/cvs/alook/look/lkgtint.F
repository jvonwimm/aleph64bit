      SUBROUTINE LKGTINT(STR,PARNUM,PROMPT,NUM,CHR)
C --------------------------------------------------------
C --------------------------------------------------------
#ifndef DOC
      CHARACTER STR*80,CHR,PAR*80,PROMPT*(*)
      INTEGER NUM
C ------------------------------------------------------
      NUM=0
      CHR=' '
      CALL LKGTPAR(STR,PARNUM,PROMPT,PAR,LEN)
      IF (LEN.EQ.1.AND.PAR(1:1).EQ.'*') THEN
          CHR='*'
      ELSEIF (LEN.GT.0) THEN
          CALL LKTRINT(PAR,NUM,CHR)
      ENDIF
      RETURN
      END
#endif
