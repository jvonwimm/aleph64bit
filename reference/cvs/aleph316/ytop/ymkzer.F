      SUBROUTINE YMKZER(MWD,MSIZ,M)
C----------------------------------------------------------*
C!    zeroes marker M
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            M     marker word
C!    output : marker M cleared
C----------------------------------------------------------*
#ifndef DOC
      DIMENSION M(*)
C
      DO 100 JWD=1,MWD
        M(JWD)=0
  100 CONTINUE
      RETURN
      END
#endif
