      SUBROUTINE YMKIDT(MWD,MSIZ,MA,MB,LID)
C----------------------------------------------------------*
C!    test if markers MA and MB are identical
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            MA,MB marker word
C!    output : LID=TRUE if markers MA and MB are identical
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION MA(*),MB(*)
      LOGICAL LID
C
      DO 100 I=1,MWD
        IF(MA(I).NE.MB(I)) THEN
          LID=.FALSE.
          RETURN
        ENDIF
C
  100 CONTINUE
      LID=.TRUE.
C
      RETURN
      END
#endif
