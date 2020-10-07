      SUBROUTINE YMKAND(MWD,MSIZ,MA,MB,M,LZERO)
C----------------------------------------------------------*
C!    logical AND of markers
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    logical AND of markers MA and MB
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            MA,MB markers
C!    output : result is stored in marker M
C!             LZERO true if no common bits are found
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION MA(*),MB(*),M(*)
      LOGICAL LZERO
C
C
      LZERO=.TRUE.
      DO 100 I=1,MWD
        M(I)=IAND(MA(I),MB(I))
        IF(M(I).NE.0) THEN
          LZERO=.FALSE.
        ENDIF
  100 CONTINUE
      RETURN
      END
#endif
