      SUBROUTINE ARPYWW

C...ARiadne subroutine PYthia WW event

C...Handle a e+e- -> W+W- or e+e- -> Z0Z0 event from PYTHIA

C allow for two steps cascade for AR0 and AR1 using MSTA(40) switch
C AR2 2steps : msta(35)=2  msta(40)=0  para(28)>0.
C AR1 2steps : msta(35)=2  msta(40)=1  para(28)>0.
C AR1 1step  : msta(35)=1  msta(40)=0  para(28)=0.
C AR0 2steps : msta(35)=0  msta(40)=2  para(28)>0.
C AR0 1step  : msta(35)=0  msta(40)=0  para(28)=0.
C
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100,MAXONI=100)

      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),PTMX2(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
      common/countev/irev
      DIMENSION IR(1)
      data nevmx/5/

      irev = irev +1
      IR(1)=0
      CALL ARSCAN(MSTI(4)+1,N,1,IR)
      MHAR(108)=1
      CALL ARCASC
      if (irev.le.nevmx) then
        print *,' stop cascade nlu= ',N
        print *,'PARA(28) MHAR(101) MHAR(111) MSTA(35) MSTA(40)',
     &             PARA(28),MHAR(101),MHAR(111),MSTA(35),MSTA(40)
      endif
C      IF (PARA(28).GT.0.0.AND.MHAR(101).EQ.2.AND.MSTA(35).EQ.2) THEN
      IF ((PARA(28).GT.0.0.AND.MHAR(101).EQ.2.AND.MSTA(35).EQ.2).or.
     $    (PARA(28).GT.0.0.AND.MSTA(40).eq.2.AND.MSTA(35).EQ.0)) THEN  
        DO 110 ID=1,IDIPS
          IF (QEM(ID)) GOTO 110
          QDONE(ID)=.FALSE.
          if(MSTA(40).eq.0) then ! AR2
            ICOLI(ID)=MOD(ICOLI(ID),1000)
          ELSEIF(MSTA(40).eq.1) then ! AR1
C         do nothing for AR1
          ELSEIF(MSTA(40).eq.2) then ! AR0
            ICOLI(ID)=0
          ENDIF
 110    CONTINUE
        IF (MHAR(111).GT.0) THEN
          PARA(28)=-PARA(28)
          PT2LST=PARA(40)
          CALL ARCONT
          PARA(28)=-PARA(28)
        ENDIF
      ENDIF
      MHAR(108)=0
      if (irev.le.nevmx) print *,' end cascade nlu= ',N
      RETURN

C**** END OF ARPYWW ****************************************************
      END
