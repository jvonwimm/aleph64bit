      SUBROUTINE YMKORR(MWD,MSIZ,MA,MB,M)
C----------------------------------------------------------*
C!    logical OR of markers
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            MA,MB markers
C!    output: logical OR of markers MA and MB
C!             result is stored in M
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION MA(*),MB(*),M(*)
C
      DO 100 I=1,MWD
        M(I)=IOR(MA(I),MB(I))
  100 CONTINUE
      RETURN
      END
#endif
