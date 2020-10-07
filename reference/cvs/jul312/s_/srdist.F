      SUBROUTINE SRDIST(TIME,DIST,IER)
C----------------------------------------------------------------------
C! Find distance track-wire
C!
C!    Author:     H. Meinhard       09-Mar-1987
C!    Modified:   H. Meinhard       07-Feb-1989
C!    Modified:   H. Meinhard       20-Jun-1991  (2)
C!                      Don't touch TIME variable
C!
C!    Input:      - TIME      /R    raw data time
C!    Output:     - DIST      /R    distance track - wire (in m)
C!                - IER       /I    Error flag: 0 = no error,
C!                                  11 = Drift time or drift dist-
C!                                  ance out of bounds, no conver-
C!                                  sion possible
C!
C!    Description
C!    ===========
C!    Convert the raw data time to drift time (taking into account the
C!    common stop mode of TDC's); Time offset TOFFSC is stored in
C!    common block SCALBC
C!    Convert drift time to drift distance by linear interpolation
C!    between two corresponding points of distance-drifttime-relation;
C!    relation is stored in SCALBC
C?
C!======================================================================
#ifndef DOC
#include "scalbc.h"
#include "srecpc.h"
C----------------------------------------------------------------------
C time conversion
      TIM = TOFFSC-TIME
      IF (TIM .LT. TLOWSR .OR. TIM .GT. TUPPSR)             GOTO 901
C
C time - to - distance - interpolation
      APOIN = (TIM-TSTASC)/TDELSC
      IF (APOIN .LT. 0. .OR. APOIN .GT. REAL(MPOISC-1))     GOTO 901
      IPOIN = INT(APOIN)+1
      IF (IPOIN.LE.0.OR.IPOIN.GE.MPOISC)                    GOTO 901
      FRAC = APOIN-FLOAT(IPOIN)+1.
      DIST = (1.-FRAC)*TSPASC(IPOIN) + FRAC*TSPASC(IPOIN+1)
      IF (DIST.GT.RUPPSR)                                   GOTO 901
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 11
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
