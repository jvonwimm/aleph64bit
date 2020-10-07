      SUBROUTINE TPSTAT
C
C-----------------------------------------------------------------------
C! Accumulate TPC cluster statistics
C!
C!    Author:   R. Johnson   04-06-87
C!    Modified: R. Johnson   19-01-88
C!
C!    Called by TPREDA
C!
C!--------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tstate.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTCLU=NAMIND('TCLU')
        NTPUL=NAMIND('TPUL')
        NTSPU=NAMIND('TSPU')
        NTSCL=NAMIND('TSCL')
      ENDIF
C
C++   Count number of clusters and pulses
C
      KTCLU=IW(NTCLU)
      KTPUL=IW(NTPUL)
   10 IF (KTCLU.EQ.0 .OR. KTPUL.EQ.0) GO TO 11
        ITSTAT(LCLUTS)=ITSTAT(LCLUTS)+IW(KTCLU+LMHROW)
        ITSTAT(LPULTS)=ITSTAT(LPULTS)+IW(KTPUL+LMHROW)
        KTCLU=IW(KTCLU-1)
        KTPUL=IW(KTPUL-1)
        GO TO 10
   11 CONTINUE
C
C++   Count number of subpulses
C
      KTSPU=IW(NTSPU)
   15 IF (KTSPU.EQ.0) GO TO 16
        ITSTAT(LTSPTS)=ITSTAT(LTSPTS)+IW(KTSPU+LMHROW)
        KTSPU=IW(KTSPU-1)
        GO TO 15
   16 CONTINUE
C
C++   Count number of subclusters
C
      KTSCL=IW(NTSCL)
   20 IF (KTSCL.EQ.0) GO TO 21
        ITSTAT(LTSCTS)=ITSTAT(LTSCTS)+IW(KTSCL+LMHROW)
        KTSCL=IW(KTSCL-1)
        GO TO 20
   21 CONTINUE
C
      RETURN
      END
#endif
