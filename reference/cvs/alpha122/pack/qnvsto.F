      SUBROUTINE QNVSTO(IFLAG,IVV,INTR,NOUT,IOUTT,XYZ,CHI2,IVOU)
CKEY FILL VERTEX   /INTERNAL
C----------------------------------------------------------------------
C!  - Stores a new vertex from new tracking
C!   Author   :  J. Boucrot                    21-Apr-1997
C!   Called from QFNINT or QFKINK
C  Input arguments:
C   IFLAG  = vertex category ( 4 for Nucl Int, 5 for Kinks)
C      IVV = Row number of the vertex in the original bank
C     INTR = ALPHA track number of incoming track, if any
C     NOUT = number of outgoing tracks
C    IOUTT = list of ALPHA numbers for the outgoing tracks
C      XYZ = vertex coordinates
C     CHI2 = chisquare of vertex fit
C
C!   Output :
C!        - new vertex IVOU in QVRT is created.
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
      INTEGER IOUTT(*)
      REAL XYZ(3)
#include "qmacro.h"
C----------------------------------------------------------------------
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
      JR(N) = KOQVEC + ITK* KCQVEC + N
C----------------------------------------------------------------------
      IF (NOUT.LE.0) GO TO 999
C
C--- Store new vertex
C
      KVRTX  = KFFRV
      IF (KFFRV .GE. KLFRV)  CALL QSBANK ('QVRT', KFFRV+200)
C
      RW(JQVRT(KVRTX,JQVRVX))=XYZ(1)
      RW(JQVRT(KVRTX,JQVRVY))=XYZ(2)
      RW(JQVRT(KVRTX,JQVRVZ))=XYZ(3)
      IW(JQVRT(KVRTX,JQVRVN))= IVV
      IW(JQVRT(KVRTX,JQVRTY))= IFLAG
      IW(JQVRT(KVRTX,JQVRIP))= INTR
      IW(JQVRT(KVRTX,JQVRND))= NOUT
      RW(JQVRT(KVRTX,JQVRCF))= CHI2
      IW(JQVRT(KVRTX,JQVRAY))=0
      IW(JQVRT(KVRTX,JQVRAF))=0
C
C---Set up mother to daughter relationship between incoming track
C---and outgoing tracks.
C
      IF (INTR.GT.0) THEN
C Set end vertex for track INTR:
         ITK=INTR
         IW(JR(JQVEEV))=KVRTX
      ENDIF
C Set mother-daughter relations for the tracks:
      IF (INTR.GT.0) THEN
         CALL QAMOTH(INTR,NOUT,IOUTT)
         CALL QADAU(INTR,NOUT,IOUTT)
      ENDIF
C Fill QLIN pointers for the new vertex:
      IF (KNQLIN+NOUT.GT.IW(KOQLIN))
     +    CALL QSBANK('QLIN',KNQLIN+NOUT+1000)
      IW(JQVRT(KVRTX,JQVRDL))=KNQLIN
      DO 450 ITT=1,NOUT
         IW(KOQLIN+KNQLIN+1)=IOUTT(ITT)
         KNQLIN=KNQLIN+1
 450  CONTINUE
C
C Set origin vertex and number of mother tracks for all outgoing tracks:
C
      DO 500 II=1,NOUT
         ITK=IOUTT(II)
         IW(JR(JQVEOV))=KVRTX
         IF (INTR.GT.0) IW(JR(JQVENO))=1
 500  CONTINUE
C Update pointers of vertex families:
      KFFRV = KFFRV + 1
      KLREV = KLREV + 1
      KNREV = KNREV + 1
      KFMCV = KFMCV + 1
      KLMCV = KLMCV + 1
C
      IVOU=KVRTX
  999 RETURN
      END
#endif
