      SUBROUTINE VCMEAN(IFS,NST,CMODE,CSIGM)
C
C!  Calculates quanitites used in removing coherent noise
C----------------------------------------------------------------------
C   Computes the average and the standard deviation on the non-flagged
C   elements of PHIN(1..NST). Returns the average as CMODE and the
C   std. dev. as CSIGM. Returns (0.,0.) if less than 2 elements found
C   non-flagged.
C   Author : C. Vannini 1/4/90
C  Modified March 1995 A. Bonissent, M. Thulasidas
C                 reorganise and debug
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "vcsgjj.h"
C----------------------------------------------------------------------
C
      INTEGER NST,IST,IFS
      INTEGER NAMIND
      INTEGER LFLAG
      LOGICAL FIRST
      REAL CMODE, CSIGM
      REAL PH
      REAL SUMX, SUMX2, CHAN
      INTEGER NCHAN, I,NAVCSG,KVCSG
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
      ENDIF
      KVCSG=IW(NAVCSG)
C
      SUMX  = 0.
      SUMX2 = 0.
      NCHAN = 0
C
      DO 20 I = 1, NST
        IST=IFS+I-1
        LFLAG=ITABL(KVCSG,IST,JVCSRF)
        IF (LFLAG.EQ.0) THEN
          PH = RTABL(KVCSG,IST,JVCSMP)
          NCHAN = NCHAN + 1
          SUMX  = SUMX  + PH
          SUMX2 = SUMX2 + PH*PH
        ENDIF
   20 CONTINUE
C   Protection from divide-by-zero's
      IF (NCHAN .GT. 1) THEN
        CHAN = NCHAN
        CMODE  = SUMX/CHAN
        CSIGM = SQRT(MAX(0.,SUMX2/(CHAN-1.)-SUMX*SUMX/(CHAN*(CHAN-1.))))
      ELSE
        CHAN = NCHAN
        CMODE = 0.0
        CSIGM = 0.0
      ENDIF
C
      RETURN
      END
#endif
