      SUBROUTINE X1HIST
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C.
C.
C! - Fill the Level1  Trigger Histograms
C.
C.
C.
C. - Called by      X1TRIG                        from this .HLB
C. - Calls          HF1                           from HBOOK
C.
#ifndef DOC
      SAVE
C.
#include "alcons.h"
#include "x1tcom.h"
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
      DO 111 L=1,NPHTR
        IF(BTEST(NTRBIT,L-1)) CALL HF1(1111,FLOAT(L),1.)
 111  CONTINUE
C
C -  Fill total energy values
C
      DO 121 L=1,NTOEV
       CALL HF1(1120+L,FLOAT(IECTTE(L))/1000.,1.)
       CALL HF1(1124+L,FLOAT(IHCTTE(L))/1000.,1.)
  121 CONTINUE
      RETURN
      END
#endif
