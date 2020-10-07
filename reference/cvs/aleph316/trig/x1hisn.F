      SUBROUTINE X1HISN
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/15  FOR ALEPHLIB 204
C.
C.
C! - Fill the Level1  Trigger Histograms
C.
C.
C.
C. - CALLED BY      X1TRIG                        FROM THIS .HLB
C -----------------------------------------------------------------
#ifndef DOC
      SAVE
C.
#include "alcons.h"
#include "x1tsto.h"
#include "bcs.h"
      LOGICAL BTEST
      DIMENSION IEQUI(2)
      EQUIVALENCE (IEQUI ,ITRG12)
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
C.
C  - Fill accepted trigger segments for each trigger
C.
      DO 101 K=1,NPHTR
        IF (K.GT.3.AND.K.NE.9) GOTO 101
        IF(IEQUI(2*K).EQ.0.AND.IEQUI(2*K-1).EQ.0)  GOTO 101
        DO 102 L=1,NFSEG
          IF (L.GT.NBITVW) THEN
            N  = 2*K - 1
            IS = L - NBITVW - 1
          ELSE
            N  = 2*K
            IS = L - 1
          ENDIF
          IF(BTEST(IEQUI(N),IS)) CALL HF1(1100+K,FLOAT(L),1.)
 102    CONTINUE
 101  CONTINUE
C
C -  Fill physics trigger bits
C
      DO 111 L=1,32
        IF(BTEST(NTRBIT,L-1)) CALL HF1(1111,FLOAT(L),1.)
 111  CONTINUE
      END
#endif
