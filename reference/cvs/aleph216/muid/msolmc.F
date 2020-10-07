      LOGICAL FUNCTION MSOLMC(ITR1)
C----------------------------------------------------------------------
C
CKEY MUONID MUON SHADOW / INTERNAL
C
C!  Find out if track ITR1 has a mu chamber hit which it shares with
C!  no other track
C!  author : G. TAYLOR        7-APR-1992
C!
C!  input : ITR1 track numbers (in JULIA)
C!  output: MSOLMC = .TRUE. if track has a non shared mu ch hit
C=======================================================================
#ifndef DOC
#include "bcs.h"
      LOGICAL SHARED
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C                check much hits in common to 2 tracks
C
      MSOLMC=.FALSE.
      JMTHR=IW(NAMIND('MTHR'))
      IF (JMTHR.LE.0) GO TO 39
      DO 31 K=1,LROWS(JMTHR)
        ITRAC=ITABL(JMTHR,K,3)
        IF (ITRAC.EQ.ITR1) THEN
          IHIT=ITABL(JMTHR,K,1)
          SHARED=.FALSE.
          DO 32 J=1,LROWS(JMTHR)
            NHIT=ITABL(JMTHR,J,1)
            NTRAC=ITABL(JMTHR,J,3)
            IF (NHIT.EQ.IHIT .AND. NTRAC.NE.ITRAC) SHARED=.TRUE.
   32     CONTINUE
          IF(.NOT.SHARED) MSOLMC=.TRUE.
        ENDIF
   31 CONTINUE
   39 CONTINUE
      RETURN
      END
#endif
