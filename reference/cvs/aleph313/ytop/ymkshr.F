      SUBROUTINE YMKSHR(MWD,MSIZ,M,IBIT)
C----------------------------------------------------------*
C!    shrinks marker M
CKEY YTOP MARKER / USER
C!    Author :     G. Lutz   30/11/87
C!    REVISED:     G.LUTZ    FEBRUARY 1991
C!
C!
C!    Description
C!    ===========
C!    shrinks marker M (take out bit IBIT)
C----------------------------------------------------------*
#ifndef DOC
C
      DIMENSION M(*)
      DIMENSION KBIT(64),KMASK(64),KMASI(64)
      LOGICAL LFRST,LBTST
C
      DATA MSIZO/0/
C
      IF(MSIZ.GT.MSIZO) THEN
        MSIZO=MSIZ
        KMASK(1)=0
        DO 10 I=2,MSIZ
          KMASK(I)=IBSET(KMASK(I-1),I-2)
          KMASI(I-1)=NOT(KMASK(I))
   10   CONTINUE
        KMASI(MSIZ)=0
      ENDIF
C
      JWD=(IBIT-1)/MSIZ+1
      JBIT=MOD((IBIT-1),MSIZ)+1
C
      M1=IAND(M(JWD),KMASK(JBIT))
      M2=IAND(M(JWD),KMASI(JBIT))
      M(JWD)=M1+ISHFT(M2,-1)
      JWD1=JWD+1
      IF(JWD1.LE.MWD) THEN
        DO 100 IWD=JWD1,MWD
          LBTST=BTEST(M(IWD),0)
          IF(LBTST) THEN
            M(IWD-1)=IBSET(M(IWD-1),MSIZ-1)
          ENDIF
          M(IWD)=ISHFT(M(IWD),-1)
  100   CONTINUE
      ENDIF
C
      RETURN
      END
#endif
