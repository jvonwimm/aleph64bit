      SUBROUTINE QWTREE (ITK,CFL)
CKEY WRITE TREE /USER
C----------------------------------------------------------------------
C! print tree of daughters for track ITK
C
C called from user routines
C                                                   H.Albrecht 20.10.88
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,CV,CP,CM,CB,CY,CZ
C
#include "qcde.h"
      DIMENSION MT(10),MD(10)
      CHARACTER * 12 CHAR
      CHARACTER * 1  C1
      CHARACTER * 2  CY(10),CZ(10),CV,CP,CM,CB,CC
      CHARACTER *(*) CFL
      DATA CV,CP,CM,CB,CY,CZ /'I ','+-','--','  ',10* '--', 10* '  '/
      DATA INIT/0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      LHEAD = -1
      IF (INDEX (CFL,'H') .NE. 0)  LHEAD = 0
      C1 = ' '
      IF (INDEX (CFL,'0') .NE. 0)  THEN
        C1 = '0'
        LHEAD = 1
      ENDIF
      IF (INDEX (CFL,'1') .NE. 0)  THEN
        C1 = '1'
        LHEAD = 1
      ENDIF
C
      IF (LHEAD .GE. 0)  WRITE (KUPRNT,1001)  C1
      CHAR = CQTPN (ITK)
C
      IVX = KORIV(ITK)
      IF (IVX .NE. 0)  THEN
        WRITE (KUPRNT,1002)
     +    ITK,KTN(ITK),CHAR,QP(ITK),QE(ITK),QM(ITK),QCH(ITK),
     +    QX(ITK),QY(ITK),QZ(ITK),IVX,QVX(IVX),QVY(IVX),QVZ(IVX)
      ELSE
        WRITE (KUPRNT,1002)
     +    ITK,KTN(ITK),CHAR,QP(ITK),QE(ITK),QM(ITK),QCH(ITK),
     +    QX(ITK),QY(ITK),QZ(ITK),IVX
      ENDIF
C
      LH = 0
      NG = 1
      NT = ITK
      CY(1) = CP
      GO TO 20
C
   10 IF (NG .NE. 10 .AND. KNDAU(NT) .NE. 0)  CY(NG+1) = CP
      IF (LH .NE. 0) WRITE (KUPRNT,1004)  CZ
      LH = 0
      CHAR = CQTPN (NT)
      IVX = KORIV(NT)
      IF (IVX .NE. 0)  THEN
        WRITE (KUPRNT,1003)
     +      NT,KTN(NT),CY,CHAR,QP(NT),QE(NT),QM(NT),QCH(NT),
     +      QX(NT),QY(NT),QZ(NT),IVX,QVX(IVX),QVY(IVX),QVZ(IVX)
      ELSE
        WRITE (KUPRNT,1003)
     +      NT,KTN(NT),CY,CHAR,QP(NT),QE(NT),QM(NT),QCH(NT),
     +      QX(NT),QY(NT),QZ(NT),IVX
      ENDIF
      CY(NG) = CC
      CZ(NG) = CC
      NG = NG + 1
      IF (NG .GT. 9)  THEN
        CALL QWMESS ('_QWTREE_ cannot print more than 9 generations')
        GO TO 60
      ENDIF
C
   20 ND = 1
   30 CC = CB
      IF (ND - KNDAU(NT))  40,50,60
C
   40 CC = CV
   50 MT(NG) = NT
      MD(NG) = ND
      NT = KDAU(NT,ND)
      GO TO 10
C
   60 NG = NG - 1
      IF (NG .EQ. 0)  GO TO 100
      IF (ND .NE. 1)  LH = 1
      CY(NG) = CP
      CY(NG+1) = CM
      CZ(NG) = CV
      CZ(NG+1) = CB
      NT = MT(NG)
      ND = MD(NG) + 1
      GO TO 30
C
  100 IF (KUPTER .NE. 0 .AND. INIT .EQ. 0)
     +    WRITE (KUPTER, 1005)
      INIT = 1
C
 1001 FORMAT (A,' ITK KTN  CQTPN',32X,'QP',6X,'QE',6X,
     +  'QM     QCH    QX',
     + 7X,'QY',7X,'QZ  KORIV    QVX',5X,'QVY QVZ(KORIV)')
 1002 FORMAT (I5,I4,1X,A,23('-'),3F8.3,F5.1,3F9.3,I4,3F8.2)
 1003 FORMAT (I5,I4,1X,10A,'-> ',A,3F8.3,F5.1,3F9.3,I4,3F8.2)
 1004 FORMAT ('  ITK KTN ',10A,18X,'QP',6X,'QE',6X,
     +  'QM     QCH    QX',
     + 7X,'QY',7X,'QZ  KORIV    QVX',5X,'QVY QVZ(KORIV)')
 1005 FORMAT (' _QWTREE_ Tree listing written to log file ')
C
      END
#endif
