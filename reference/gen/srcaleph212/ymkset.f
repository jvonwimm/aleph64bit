      SUBROUTINE YMKSET(MWD,MSIZ,M,IBIT)
C----------------------------------------------------------*
C!    set bit I in marker M
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!
C!
C!    Description
C!    ===========
C!    input : MWD   nb of marker words
C!            MSIZ  size of marker word
C!            M     marker word
C!            IBIT  bit nb to be set in marker M
C!    output : modified marker M
C----------------------------------------------------------*
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C----------------------------------------------------------*
C
      DIMENSION M(*)
C
C-- Define the logical unit for printout
      LOUT = IW(6)
C
      JWD=(IBIT-1)/MSIZ+1
C
      IF(JWD.GT.MWD) THEN
        WRITE(LOUT,10) MWD,MSIZ,IBIT
   10   FORMAT(/' ***** ILLEGAL CALL TO YMKSET: MWD=',I5,
     &    ' MSIZ=',I5,' IBIT=',I5)
        STOP
      ENDIF
C
      JBIT=MOD((IBIT-1),MSIZ)
      M(JWD)=IBSET(M(JWD),JBIT)
      RETURN
      END
