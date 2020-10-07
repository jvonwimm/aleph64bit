      SUBROUTINE YMKTST(MWD,MSIZ,M,IBIT,LSET)
C----------------------------------------------------------*
C!    test if bit IBIT in marker M is set
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            M     marker word
C!            IBIT  bit nb to be checked in marker M
C!    output : LSET=TRUE if bit is set
C----------------------------------------------------------*
#ifndef DOC
      DIMENSION M(*)
      LOGICAL LSET, BTEST
C
      JWD=(IBIT-1)/MSIZ+1
      JBIT=MOD((IBIT-1),MSIZ)
      LSET=BTEST(M(JWD),JBIT)
      RETURN
      END
#endif
