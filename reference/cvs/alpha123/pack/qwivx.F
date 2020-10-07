      SUBROUTINE QWIVX (IVX,CFL)
CKEY WRITE VERTEX /USER
C----------------------------------------------------------------------
C! print vertex IVX
C
C called from user routines
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,CTEXT
#include "qcde.h"
      CHARACTER * 1  C1
      CHARACTER * 12 CHAR
      CHARACTER *(*) CFL
      CHARACTER * 24 CTEXT(2)
      character*18 blaver(6)
      data blaver / 'Stand. V0 Vertex  ' , 'Main Vertex       ' ,
     +              'Vertex from KVFITx' , 'Long V0 Vertex    ' ,
     +              'Nucl. Int. Vertex ' , 'Vertex from Kink  ' /
      DATA CTEXT /'Reconstructed vertices', 'MC truth (vertices)'/
      DATA INIT /0/
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
      DO 10 NU=KSREV,KSMCV
   10 IF (IVX .GE. KLOCUS(1,NU) .AND. IVX .LE. KLOCUS(2,NU))
     +  NSEC = NU
C
      CHAR = ' '
      ITK = KVINCP(IVX)
      IF (ITK .NE. 0)  CHAR = CQTPN (ITK)
      ITYP= KVTYPE(IVX)
      ICHOI = NSEC-KSREV+1
 
C Write header if needed:
      IF (LHEAD .GT. 0)  WRITE (KUPRNT,1001)
     +       C1,CQSEC(NSEC),CTEXT(ICHOI)
      IF (LHEAD.GE.0.AND.ICHOI.EQ.2)  WRITE (KUPRNT,1002)
      IF (LHEAD.GE.0.AND.ICHOI.EQ.1)  WRITE (KUPRNT,2002)
C Write vertex debug:
      IF (ICHOI.EQ.2) WRITE (KUPRNT,1003)
     +       IVX,KVN(IVX),QVX(IVX),QVY(IVX),QVZ(IVX),
     +       KVNDAU(IVX),ITK,CHAR
      IF (ICHOI.EQ.1) WRITE (KUPRNT,2003)
     +       IVX,KVN(IVX),QVX(IVX),QVY(IVX),QVZ(IVX),
     +       KVNDAU(IVX),ITK,ITYP,BLAVER(ITYP+1),CHAR
      IF (KUPTER .NE. 0 .AND. INIT .EQ. 0)
     +    WRITE (KUPTER, 1004)
      INIT = 1
C
 1001 FORMAT (A,'section KS',A,1X,A)
 1002 FORMAT ('  IVX KVN    QVX',9X,'QVY',9X,'QVZ',5X,
     +  'KVNDAU KVINCP CQTPN(KVINCP)')
 1003 FORMAT (I5,I4,3F12.6,2I6,2X,A)
 1004 FORMAT (' _QWIVX_ Vertex listing written to log file ')
 2002 FORMAT ('  IVX KVN    QVX',9X,'QVY',9X,'QVZ',5X,
     +  'KVNDAU KVINCP ITYP VERTEX TYPE       CQTPN(KVINCP)')
 2003 FORMAT (I5,I4,3F12.6,2I6,3X,I3,1X,A18,2x,a12)
C
      END
#endif
