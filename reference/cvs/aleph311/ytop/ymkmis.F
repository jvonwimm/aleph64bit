      SUBROUTINE YMKMIS(MWD,MSIZ,MTEST,MSET,LMISS)
C----------------------------------------------------------*
C!    compares marker MSET and marker MTEST
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of makrker word
C!            MTEST reference marker word
C!            MSET  marker word to be tested
C!    output : LMISS=TRUE if bits do not compare
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION MTEST(*),MSET(*)
      LOGICAL LMISS
C
      DO 100 JWD=1,MWD
        MMISS=MTEST(JWD)-IAND(MSET(JWD),MTEST(JWD))
        IF(MMISS.NE.0)  THEN
          LMISS=.TRUE.
          RETURN
        ENDIF
C
  100 CONTINUE
      LMISS=.FALSE.
      RETURN
      END
#endif
