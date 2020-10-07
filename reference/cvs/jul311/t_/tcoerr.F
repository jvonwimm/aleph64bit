      SUBROUTINE TCOERR(ERRM,NPAD,WP,WZ,IQ)
C-----*************************************--------------------------C
C! Calculate the errors on tpc pad coordinates
C!
C!    Author: Martin Poppe
C!    Date  : August 1987
C!    Modified:  R. Johnson   7-02-88
C!
C!    INPUT:  NPAD = Number of pads contributing to subcluster
C!            WP   = Sub cluster width in R*PHI
C!            WZ   = Sub cluster widths in Z
C!            IQ   = total charge of subcluster
C!       ---- All input units as in 'TSCL' ---
C!
C!    OUTPUT: SP2  = SIGMA**2(R*PHI)  [CM**2]  = ERRM(1)
C!            COV  = COV (R*PHI,Z)    [CM**2]  = ERRM(2)
C!            SZ2  = SIGMA**2(Z)      [CM**2]  = ERRM(3)
C!
C!    This routine will be replaced by a database driven one after
C!    the final choice of the coordinate determination algorithm
C-------------------------------------------------------------------C
#ifndef DOC
C
#include "tcercl.h"
C
      DIMENSION ERRM(*)
C
      DATA FUDGE/0.95/
C
C++   Calculate the r*phi error:
C
      DO 10 I=NPRPER,1,-1
        IF (WP.GT.XBRPER(I)) THEN
          SIGP= CFRPER(1,I)*WP + CFRPER(2,I)
          GO TO 11
        ENDIF
   10 CONTINUE
      SIGP=CFRPER(2,1)
   11 CONTINUE
      SIGP= SIGP * FUDGE
      ERRM(1)=SIGP*SIGP
C
C++   Calculate the z error:
C
      DO 20 I=NPZDER,1,-1
        IF (WZ.GT.XZRPER(I)) THEN
          SIGZ= CFZDER(1,I)*WZ + CFZDER(2,I)
          GO TO 21
        ENDIF
   20 CONTINUE
      SIGZ=CFZDER(2,1)
   21 CONTINUE
      SIGZ= SIGZ * FUDGE
      ERRM(3) = SIGZ*SIGZ
C
C++   The correlation is assumed to be zero
C
      ERRM(2) = 0.0
C
      RETURN
      END
#endif
