      SUBROUTINE LKGTPAR(STR,PARNUM,PROMPT,PAR,LEN)
C -----------------------------------------------------------
C -----------------------------------------------------------
#ifndef DOC
      CHARACTER STR*80,PAR*80,PROMPT*(*)
      INTEGER PARNUM,PN
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      INTEGER LIB$GET_COMMAND
#endif
C ------------------------------------------------------------
      PN=1
      LEN=0
      PAR=' '
      I=0
 10   I=I+1
      IF (STR(I:I).EQ.' ') THEN
          IF (I.LT.79) GOTO 10
      ELSEIF (STR(I:I).EQ.'/') THEN
          IL=LKLWRD(STR(I+1:80))
          I=I+IL
          IF (I.LT.77) GOTO 10
      ELSE
          LEN=LKLWRD(STR(I:80))
          IF (PN.LT.PARNUM) THEN
              PN=PN+1
              I=I+LEN-1
              LEN=0
              GOTO 10
          ELSE
              PAR=STR(I:I+LEN-1)
              IF (PAR(1:1).EQ.'"') THEN
                  PAR=PAR(2:LEN-1)
                  LEN=LEN-2
              ENDIF
          ENDIF
      ENDIF
 100  IF (PAR.EQ.' '.AND.PROMPT.NE.' ') THEN
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
          I=LIB$GET_COMMAND(PAR,PROMPT)
#else
          CALL LKREAD0 (PROMPT,' ',PAR)
#endif
          LEN=LNBLNK(PAR)
#ifndef UNIX
          CALL CLTOU(PAR)
#endif
          GOTO 100
      ENDIF
      RETURN
      END
#endif
