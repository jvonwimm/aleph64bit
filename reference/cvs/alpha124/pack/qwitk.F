      SUBROUTINE QWITK (ITK,CFL)
CKEY WRITE TRACKS /USER
C----------------------------------------------------------------------
C! print track ITK
C
C called from user routines
C                                                   H.Albrecht 20.09.88
C                                    Modified:      E.Blucher  02.02.92
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,CTEXT
#include "qcde.h"
      CHARACTER * 70  CTEXT(12)
      CHARACTER * 12 CHAR,CTIT
      CHARACTER * 1  C1
      CHARACTER *(*) CFL
      DATA INIT /0/
      DATA CTEXT /'Neutral objects overlapping with other objects',
     +       'Charged reconstructed tracks',
     +       'Isolated neutral CAL objects',
     +       'CAL objects associated to other reconstructed objects',
     +       'ECAL/HCAL objects contributing to composite CAL objects',
     +       'Neutral decaying tracks with reconstructed decay vertex',
     +       'Charged decay tracks from a reconstructed decay vertex',
     +       'Energy flow objects', 'Neutral Objects from PCPA',
     +       'Photons from GAMPEC','Reconstructed Jets','MC truth'/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      IF (XLOCK(ITK) .AND. INDEX (CFL,'L') .NE. 0)  GO TO 90
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
      LNAME = 1
      CTIT = 'CQTPN'
      IF (ITK .GE. KFOVT .AND. ITK .LT. KFMCT) THEN
        LNAME = 0
        CTIT = 'det. data'
      ENDIF
C
      DO 10 NU=1,KSMCT
   10 IF (ITK .GE. KLOCUS(1,NU) .AND. ITK .LE. KLOCUS(2,NU))
     +  NSEC = NU
C
      IF (LHEAD .GT. 0)  WRITE (KUPRNT,1001)
     +    C1,CQSEC(NSEC),CTEXT(NSEC)
      IF (LHEAD .GE. 0)  WRITE (KUPRNT,1002)  CTIT
C
      IF (LNAME .GT. 0)  THEN
        CHAR = CQTPN (ITK)
      ELSE
        CHAR = ' '
        IF (XFRF(ITK))  CHAR(1:1) = 'F'
        IF (XTEX(ITK))  CHAR(2:2) = 'T'
        IF (XHMA(ITK))  CHAR(3:3) = 'H'
        IF (XMCA(ITK))  CHAR(4:4) = 'M'
        IF (CHAR .EQ. ' ')  CHAR = CQTPN (ITK)
        IF (KNHCAL(ITK) .NE. 0)  CHAR(11:11) = 'H'
        IF (KNECAL(ITK) .NE. 0)  CHAR(10:10) = 'E'
        IF (KNCHGD(ITK) .NE. 0)  CHAR( 9: 9) = 'C'
      ENDIF
C
      VX = 0.
      VY = 0.
      VZ = 0.
      IVX = KORIV(ITK)
      IF (IVX .NE. 0) THEN
        VX = QVX(IVX)
        VY = QVY(IVX)
        VZ = QVZ(IVX)
      ENDIF
C
      WRITE (KUPRNT,1003)
     +  ITK,KTN(ITK),CHAR,QP(ITK),QE(ITK),QM(ITK),QCH(ITK),
     +  QX(ITK),QY(ITK),QZ(ITK),KENDV(ITK),KNMOTH(ITK),KORIV(ITK),
     +  QDB(ITK),QZB(ITK),VX,VY,VZ
      IF(XSIG(ITK))  WRITE (KUPRNT,1004)  QSIGP(ITK),QSIGE(ITK),
     +  QSIGM(ITK),QSIGX(ITK),QSIGY(ITK),QSIGZ(ITK),QSQT(QDBS2(ITK)),
     +  QSQT(QZBS2(ITK))
      IF (KUPTER .NE. 0 .AND. INIT .EQ. 0)
     +    WRITE (KUPTER,1005)
      INIT = 1
C
 1001 FORMAT (A,'section KS',A,1X,A)
 1002 FORMAT ('  ITK KTN ',A,'   QP',6X,'QE',6X,'QM     QCH    QX',
     + 7X,'QY',7X,'QZ KENDV KNMOTH KORIV QDB   QZB',
     + 5X,'QVX',5X,'QVY QVZ(KORIV)')
 1003 FORMAT (I5,I4,1X,A,3F8.3,F5.1,3F9.3,I4,I5,I4,2F7.2,3F8.2)
 1004 FORMAT (19X,'+- ',3F8.3,5X,3F9.3,13X,2F7.2)
 1005 FORMAT (' _QWITK_ Track listing written to log file ')
C
   90 END
#endif
