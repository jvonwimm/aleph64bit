C!======================================================================
      SUBROUTINE PR_SCAL(IOP,X1,X2,X3,Y1,Y2,Y3,ZCOS)
C!======================================================================
#ifndef DOC
#include "qcde.h"
      INTEGER IOP
      REAL    X1,X2,X3,Y1,Y2,Y3,XX,YY,ZCOS
#include "qmacro.h"
C!======================================================================
C!.ON DEFINI LE COS DE L'ANGLE ENTRE XX ET YY  :  ZCOS
C!.AVEC 2 OPTIONS : IOP = 1 (2) NORMALISE ( OU PAS)
C**
      XX=SQRT(X1**2+X2**2+X3**2)
      YY=SQRT(Y1**2+Y2**2+Y3**2)
      IF(XX.EQ.0.0.OR.YY.EQ.0.0) THEN
        ZCOS  = 999.
        RETURN
      ENDIF
C**
      ZCOS=X1*Y1+X2*Y2+X3*Y3
C**
      IF(IOP.EQ.1) THEN
        ZCOS=ZCOS/(XX*YY)
      ELSE
        ZCOS=ZCOS
      ENDIF
C**
      RETURN
      END

*AF
#endif
