*DK DT_WRT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DT_WRT
CH
      SUBROUTINE DT_WRT(T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      CHARACTER *60 TREP
      DATA TREP/
     &  'Reply on the keyboard by typing <CR>=return or one letter:'/
      DATA HT1/5./,HT2/400./,ISL/5./

      IF(T.NE.' ') TXTADW=T
      CALL DO_BAR_ANSWER(HT1,HT2,TREP,ISL)
      CALL DWRC
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------------  DT_BAR
CH
      ENTRY DT_BAR(T)
CH
CH --------------------------------------------------------------------
CH
      IF(T.NE.' ') TXTADW=T
      CALL DO_BAR_ANSWER(HT1,HT2,TREP,ISL)
      END
*DK DTNOSL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTNOSL
CH
      SUBROUTINE DTNOSL(P,I,N,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      CHARACTER *(*) T
      DIMENSION P(4,1)
      IF(P(4,I)) 1,2,3
    1 T(N:N)='/'
      RETURN
    2 T(N:N)='='
      RETURN
    3 T(N:N)=':'
      END
*DK DTYANS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTYANS
CH
      SUBROUTINE DTYANS(T,TR,N)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INPUT: T=TEXT TO TYPE, TR=1 WORD OF ACCEPTED LETTERS TO ANSWER
C     OUTPUT: N=-1 ANSWER NOT IN TR, but stored in TXTADW
C             N= 0 ANSWER = RETURN
C             ELSE N = # OF LETTER IN TR
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T,TR,TX
      CHARACTER *1 TA
      CHARACTER *49 TW

      L=LENOCC(T)
      IF(L.NE.2) THEN
        TW=T
      ELSE
        CALL DW_GET_PLATFORM_TEXT(T,TW,49)
      END IF
      CALL DT_WRT(TW)
      L=LENOCC(TR)
      IF(TR.EQ.'<') THEN
        CALL DGETLN(TA,NA,0)
      ELSE
        CALL DGETLN(TA,NA,1)
      END IF
      IF(NA.EQ.1) THEN
        TXTADW=TA
        DO N=1,L
          IF(TA.EQ.TR(N:N)) RETURN
        END DO
        N=-1
      ELSE
        N=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTYATX
CH
      ENTRY DTYATX(TX)
CH
CH --------------------------------------------------------------------
CH
      TX=TA
      END
*DK DTN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTN
CH
      SUBROUTINE DTN(F,N,TOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Change to special format
C    Inputs    :F=FLOATING POINT #, N=#OF LETTERS IN OUTPUT
C    Outputs   :TOUT=CHARACTER STRING
C
C ---------------------------------------------------------------------
      CHARACTER *(*) TOUT
      CHARACTER *17 T17
      CHARACTER *7 TF(2:9)
      DATA TF/
     &  '(F3.1)',
     &  '(F5.2)',
     &  '(F7.3)',
     &  '(F9.4)',
     &  '(F11.5)',
     &  '(F13.6)',
     &  '(F15.7)',
     &  '(F17.8)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      DIMENSION FMIN(9),FMAX(9)
      DATA FMIN/-0.5,-9.5,-99.5,-999.5,-9999.5,-99999.5,-999999.5,
     &  -9999999.5,-99999999.5/
      DATA FMAX/ 9.5,99.5,999.5,9999.5,99999.5,999999.5,9999999.5,
     &  99999999.5,999999999.5/
C     .................................. EXCLUDE TOO BIG AND TOO SMALL NUMBERS
      IF(F.GE.FMAX(N).OR.F.LE.FMIN(N)) GO TO 99
      J=N-1
C     .............................. IF NUMBERS ARE TOO BIG CONVERT TO INTEGER
      IF(N.EQ.1.OR.F.GE.FMAX(J).OR.F.LE.FMIN(J)) THEN
        I=NINT(F)
        GO TO 10
      END IF
      I=F
      G=I
C     ......................... IF NUMBER IS ZERO AFTER "," CONVERT TO INTEGER
      IF(G.EQ.F) GO TO 10
C     ..... ONLY POSITIVE NUMBERS ARE HANDLED, THE "-" IS INSTALLED AT THE END
      FA=ABS(F)
      IF(F.GE.0.) THEN
        K=N
      ELSE
        K=N-1
      END IF
      IF(FA.GE.0.1) THEN
C       .......................... USE G-FORMAT WHER IT DOES NOT GIVE EXPONENT
        WRITE(T17,TG(K),ERR=99) FA
      ELSE
C       ..                                  . USE F-FORMAT FOR SMALLER NUMBERS
        WRITE(T17,TF(K),ERR=99) FA
      END IF
C     ............................................................ ADJUST LEFT
      DO 701 L=1,17
        IF(T17(L:L).NE.' ') GO TO 21
  701 CONTINUE
   21 IF(K.EQ.N) THEN
C       ..................................................... POSITIVE NUMBERS
        IF(T17(L:L).NE.'0') THEN
          TOUT=T17(L:L+K-1)
        ELSE
C         ........................................... CHANGE "0.XXX" TO ".XXX"
          TOUT=T17(L+1:L+K)
        END IF
      ELSE
C       ..................................................... NEGATIVE NUMBERS
        IF(T17(L:L).NE.'0') THEN
          TOUT='-'//T17(L:L+K-1)
        ELSE
C         ........................................... CHANGE "0.XXX" TO ".XXX"
          TOUT='-'//T17(L+1:L+K)
        END IF
      END IF
      RETURN
   10 WRITE(T17,1010,ERR=99) I
 1010 FORMAT(I9)
C     .......................................................... ADJUST LEFT
      DO 700 L=1,17
        IF(T17(L:L).NE.' ') GO TO 11
  700 CONTINUE
   11 TOUT=T17(L:L+J)
      RETURN
C     ............................................................... OVERFLOW
   99 TOUT='*'
      RETURN
      END
*DK DTP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTP
CH
      SUBROUTINE DTP(F,N,TOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      CHARACTER *(*) TOUT
      IF(F.GT.0.) THEN
         TOUT(1:1)='+'
         CALL DTN(F,N-1,TOUT(2:N))
      ELSE
         CALL DTN(F,N,TOUT)
      END IF
      END
*DK DT1
      CHARACTER *(*) FUNCTION DT1(F)
      CHARACTER *1 DTT
      CALL DTN(F,1,DTT)
      DT1=DTT
      END
*DK DT5
      CHARACTER *(*) FUNCTION DT5(F)
      CHARACTER *5 DTT
      CALL DTN(F,5,DTT)
      DT5=DTT
      END
*DK DT2
      CHARACTER *(*) FUNCTION DT2(F)
      CHARACTER *2 DTT
      CALL DTN(F,2,DTT)
      DT2=DTT
      END
*DK DT3
      CHARACTER *(*) FUNCTION DT3(F)
      CHARACTER *3 DTT
      CALL DTN(F,3,DTT)
      DT3=DTT
      END
*DK DT3Z
      CHARACTER *(*) FUNCTION DT3Z(F)
      CHARACTER *3 DTT
      IF(F.EQ.0.) THEN
        DT3Z='   '
      ELSE
        CALL DTN(F,3,DTT)
        DT3Z=DTT
      END IF
      END
*DK DT4
      CHARACTER *(*) FUNCTION DT4(F)
      CHARACTER *4 DTT
      CALL DTN(F,4,DTT)
      DT4=DTT
      END
*DK DT6
      CHARACTER *(*) FUNCTION DT6(F)
      CHARACTER *6 DTT
      CALL DTN(F,6,DTT)
      DT6=DTT
      END
*DK DT7
      CHARACTER *(*) FUNCTION DT7(F)
      CHARACTER *7 DTT
      CALL DTN(F,7,DTT)
      DT7=DTT
      END
*DK DT8
      CHARACTER *(*) FUNCTION DT8(F)
      CHARACTER *8 DTT
      CALL DTN(F,8,DTT)
      DT8=DTT
      END
*DK DTINT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTINT
CH
      SUBROUTINE DTINT(I,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Integer I is filled left adjusted into character string T(N1:N2)
C
      CHARACTER *(*) T
      CHARACTER *1 T1(0:9)
      DATA T1/'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER *18 TW
      IF(N1.EQ.N2) THEN
        IF(I.GE.0.AND.I.LE.9) THEN
          T(N1:N2)=T1(I)
        ELSE
          T(N1:N2)='*'
        END IF
        RETURN
      END IF
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,1000,ERR=9) I
 1000 FORMAT(I18)
      DO L=1,18
        IF(TW(L:L).NE.' ') THEN
          IF(N2-N1.LT.18-L) GO TO 9
          T(N1:N2)=TW(L:18)
          RETURN
        END IF
      END DO
    9 T(N1:N2)=' '
      IF(N1.EQ.N2.OR.I.GT.0) THEN
        T(N1  :N1)='*'
      ELSE
        T(N1:N1+1)='-*'
      END IF
      END
*DK DTINTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTINTR
CH
      SUBROUTINE DTINTR(I,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Integer I is filled right adjusted into character string T(N1:N2)
C
      CHARACTER *(*) T
      CHARACTER *19 TW
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,1000,ERR=9) I
 1000 FORMAT(I18)
      IF(TW(18-ND:18-ND).NE.' ') GO TO 9
      T(N1:N2)=TW(18-ND+1:18)
      RETURN
    9 T(N1:N2)=' '
      IF(N1.EQ.N2.OR.I.GT.0) THEN
        T(N2  :N2)='*'
      ELSE
        T(N2-1:N2)='-*'
      END IF
      END
*DK DTFLR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTFLR
CH
      SUBROUTINE DTFLR(R,TF,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Real R is filled right adjusted into character string T(N1:N2)
C     with Format TF
C                      Example:      CALL DTFLR(R,'(F5.2)',4,8,T)
C
      CHARACTER *(*) T,TF
      CHARACTER *19 TW
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,TF,ERR=9) R
      T(N1:N2)=TW(1:ND)
      RETURN
    9 T(N1:N2)=' '
      T(N2:N2)='*'
      IF(N2.NE.N1) THEN
        IF(R.GE.0) THEN
          T(N2-1:N2)='+*'
        ELSE
          T(N2-1:N2)='-*'
        END IF
      END IF
      END

*DK DTR_I
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTR_I
CH
      SUBROUTINE DTR_I(T,L1,LT,I)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     read integer I from T(L1:LT) before next blanc.

      CHARACTER *(*) T
      L2=L1+INDEX(T(L1:LT),' ')-2
      READ(T(L1:L2),1000,ERR=9) I
 1000 FORMAT(I12)
      L1=L2+2
      RETURN
    9 I=1
      L1=L2+2
      END

*DK DTR_F
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTR_F
CH
      SUBROUTINE DTR_F(T,L1,LT,R)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     read real R from T(L1:LT) before next blanc.

      CHARACTER *(*) T
      L2=L1+INDEX(T(L1:LT),' ')-2
      READ(T(L1:L2),1000,ERR=9) R
 1000 FORMAT(F15.8)
      L1=L2+2
      RETURN
    9 R=1.
      L1=L2+2
      END


*DK DTR_T
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTR_T
CH
      SUBROUTINE DTR_T(T,L1,LT,LTX,TXT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     read text TXT from T(L1:LT) before next blanc with LTXT letters

      CHARACTER *(*) T,TXT

      CHARACTER *5 TF
      DATA TF/'(A12)'/

      WRITE(TF(3:4),1000,ERR=9) LTX
 1000 FORMAT(I2)

      L2=L1+LTX-1
      READ(T(L1:L2),TF,ERR=9) TXT
      L1=L2+2
      RETURN
    9 TXT=' '
      L1=L2+2
      END

