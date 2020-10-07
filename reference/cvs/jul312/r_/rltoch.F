      SUBROUTINE RLTOCH(IW,LIST,STRING)
C----------------------------------------------------------------------
C!  - Move BOS bank names from list LIST into character string
C?
C!======================================================================
#ifndef DOC
      CHARACTER*(*) LIST,STRING
      CHARACTER*4 NAME,NLIST
      INTEGER IW(*)
      I=0
    1 I=I+1
      NAME=NLIST(IW,I,LIST)
      IF(NAME.EQ.'    ')GOTO 999
      STRING=STRING(1:LNBLNK(STRING))//NAME
      GOTO 1
  999 RETURN
      END
#endif
