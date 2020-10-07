      SUBROUTINE OMBOPT
C----------------------------------------------------------------------
C!  - Calculate optics matrices
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
#ifndef DOC
#include "bomopt.h"
C
      INTEGER KC, KAB, IFAIL, IZ, I, J, KA, KB, IDPA
      REAL VA(16)
C.......................................................................
C
C  Build P and M matrices
C
      DO 200 KAB = 1, 2
        CALL UCOPY (RBIOPT(1,1,KAB), PIBOPT(1,1,KAB), 16)
        CALL RINV (4, PIBOPT(1,1,KAB), 4, VA, IFAIL)

        DO 100 KC = 1, NUMCOR
          CALL RMMLT (4, 4, 4, PIBOPT(1,1,KAB), PIBOPT(1,2,KAB),
     &                PIBOPT(2,1,KAB), RCIOPT(1,1,KAB,KC),
     &                RCIOPT(1,2,KAB,KC),
     &                RCIOPT(2,1,KAB,KC), AMCIOP(1,1,KAB,KC),
     &                AMCIOP(1,2,KAB,KC), AMCIOP(2,1,KAB,KC), VA)
  100   CONTINUE
  200 CONTINUE
C
      DO 500 IDPA = 1, 2
C
C  IDPA = 1 analyze e+
C         2         e-
C
        IF (IDPA .EQ. 1) THEN
          KA = 2
          KB = 1
        ELSE
          KA = 1
          KB = 2
        ENDIF
C
        DO 400 IZ = 1, 2
          DO 300 J = 1, 4
            ZMOPTI(IZ,J,IDPA)   = PIBOPT((IZ-1)*2+1,J,KB)
            ZMOPTI(IZ+2,J,IDPA) = RIBOPT((IZ-1)*2+1,J,KA)
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
C
      RETURN
      END
#endif
