      SUBROUTINE RSHRIN(IW,LIST,STRING)
C----------------------------------------------------------------------
C!  - Move BOS bank names from list LIST into character string
C     FOR EXISTING BANKS ONLY
C?
C!======================================================================
#ifndef DOC
      CHARACTER*(*) LIST,STRING
      CHARACTER*4 NAME,NLIST
      INTEGER IW(*)
      INTEGER LONG
      LONG = 0
      I=0
      STRING=' '
    1 I=I+1
      NAME=NLIST(IW,I,LIST)
      IF(NAME.EQ.'    ')GOTO 999
      IF(IW(NAMIND(NAME)).NE.0) THEN
        IF( LONG .GT. 0 ) THEN
          STRING = STRING(1:LONG)//NAME
        ELSE
          STRING = NAME
        ENDIF
        LONG = LONG + 4
      ENDIF
      GOTO 1
  999 RETURN
      END
#endif
