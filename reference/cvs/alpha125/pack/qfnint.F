      SUBROUTINE QFNINT
CKEY FILL VERTEX   /INTERNAL
C----------------------------------------------------------------------
C!  - Fill parameters of Nuclear Interaction vertices from new tracking
C!  - banks YNLI and YNVH.
C!
C!   Author   :- J. Boucrot                    21-Apr-1997
C!
C!   Calls QFYNVH,QVNSTO
C!   Output :
C!        - new vertex in QVRT is created.
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "ynlijj.h"
      PARAMETER ( ITYVR = 4 , NOUTT = 50 )
      INTEGER IOUTT(NOUTT)
      REAL XYZ(3)
#include "qmacro.h"
C----------------------------------------------------------------------
C--- Check if bank of nuclear interactions is present :
C
      JYNLI=IW(NAMIND('YNLI'))
      IF (JYNLI.LE.0) GO TO 999
C Loop on nuclear interaction vertices (usually only 1) :
      DO 600 IVV=1,LROWS(JYNLI)
         KYNLI=KROW(JYNLI,IVV)
         CALL VZERO(IOUTT,NOUTT)
C
C--- Get list of incoming/outgoing tracks from bank 'YNVH':
C
         CALL QFYNVH(IVV,INTR,NOUT,IOUTT)
         IF (NOUT.LE.0) GO TO 600
C
C--- Store new vertex
C
         XYZ(1)=RW(KYNLI+JYNLXP)
         XYZ(2)=RW(KYNLI+JYNLYP)
         XYZ(3)=RW(KYNLI+JYNLZP)
         IFLAG =ITYVR
         CHI2  =RW(KYNLI+JYNLCH)
         CALL QNVSTO(IFLAG,IVV,INTR,NOUT,IOUTT,XYZ,CHI2,IVOU)
  600 CONTINUE
C
  999 RETURN
      END
#endif
