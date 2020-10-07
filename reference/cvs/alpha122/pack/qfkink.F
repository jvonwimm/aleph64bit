      SUBROUTINE QFKINK
CKEY FILL VERTEX   /INTERNAL
C----------------------------------------------------------------------
C!  - Fill parameters of Kink vertices from new tracking
C!  - bank YKNK
C!
C!   Author   :  J. Boucrot                    21-Apr-1997
C!   Calls QNVSTO
C!   Output :
C!        - new vertex in QVRT is created.
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "yknkjj.h"
      PARAMETER ( ITYVR = 5 )
      INTEGER LISTO(2)
      REAL XYZ(3)
#include "qmacro.h"
C----------------------------------------------------------------------
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
C----------------------------------------------------------------------
C--- Check if bank of kinks is present :
C
      JKNKI=IW(NAMIND('YKNK'))
      IF (JKNKI.LE.0) GO TO 999
C
C Loop on kink vertices (usually only 1) :
C
      DO 600 IVV=1,LROWS(JKNKI)
         KKNKI=KROW(JKNKI,IVV)
         CALL VZERO(LISTO,2)
C
C--- Store new vertex
C
         XYZ(1)=RW(KKNKI+JYKNVX)
         XYZ(2)=RW(KKNKI+JYKNVX+1)
         XYZ(3)=RW(KKNKI+JYKNVX+2)
         INTR  =IW(KKNKI+JYKNIT)+KFCHT-1
         IFLAG =ITYVR
         CHI2  =RW(KKNKI+JYKNC2)
         NOUT  =1
         LISTO(NOUT) = IW(KKNKI+JYKNOT)+KFCHT-1
         CALL QNVSTO(IFLAG,IVV,INTR,NOUT,LISTO,XYZ,CHI2,IVOU)
         IF (IVOU.LE.KFREV) GO TO 600
C Store some important kink parameters in the vertex user's words:
         RW(JQVRT(IVOU,JQVRET+ 9))=RW(KKNKI+JYKNPK)
         RW(JQVRT(IVOU,JQVRET+10))=RW(KKNKI+JYKNPT)
         RW(JQVRT(IVOU,JQVRET+11))=RW(KKNKI+JYKNPL)
         IW(JQVRT(IVOU,JQVRET+12))=IW(KKNKI+JYKNBH)
C Store the error matrix:
         DO 500 IEM=1,6
            RW(JQVRT(IVOU,JQVREM+IEM-1))=RW(KKNKI+JYKNVC+IEM-1)
  500    CONTINUE
  600 CONTINUE
C
  999 RETURN
      END
#endif
