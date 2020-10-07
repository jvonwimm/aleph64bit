      SUBROUTINE YMKCOP(MWD,M,MCOP)
C----------------------------------------------------------*
C!    copies marker M to MCOP
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            M     marker word
C!    output : MCOP copy of M
C----------------------------------------------------------*
#ifndef DOC
      DIMENSION M(*),MCOP(*)
C
      DO 100 JWD=1,MWD
        MCOP(JWD)=M(JWD)
  100 CONTINUE
      RETURN
      END
#endif
