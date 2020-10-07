      SUBROUTINE YMKEXP(MWD,MSIZ,M,MAXBT,N,JBIT)
C----------------------------------------------------------*
C!    expand marker M up to bit MAXBT
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            M     marker word
C!            MAXBT maximum nb of bits to consider
C!    output : nb of set bits in M up to MAXBT in N
C!             set bit indices in JBIT(1 to N)
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION M(*),JBIT(*)
      LOGICAL LBTST, BTEST
C
      N=0
      IBIT=0
      DO 100 JWD=1,MWD
        MM=M(JWD)
C
        IF(MM.EQ.0) THEN
          IBIT=IBIT+MSIZ
          GO TO 100
        ENDIF
C
        DO 50  I=1,MSIZ
          IBIT=IBIT+1
          LBTST=BTEST(MM,I-1)
          IF(LBTST) THEN
            N=N+1
            JBIT(N)=IBIT
          ENDIF
C
          IF(IBIT.GE.MAXBT) GO TO 300
   50   CONTINUE
  100 CONTINUE
  300 CONTINUE
      RETURN
      END
#endif
