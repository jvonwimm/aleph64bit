      SUBROUTINE LBHAB
C-----------------------------------------------------------------------
C! Luminosity measurement
C!
C!   Author   : P. H. Hansen  890101
C!   Modified : P. H. Hansen  890701, 910801
C!
C!     Input  :  ITYPE           Trigger type
C!                   1 : exclusive single low
C!                   2 : exclusive single high
C!                   3 : exclusive single very high
C!                   4 : exclusive low-low
C!                   5 : high-low
C!              'LIDT'           Electron candidate clusters
C!              'LIFL'           Selection flags for electron candidates
C!     Output : 'LUMI'           Bhabha event statistics
C!              'LSIN'           Single arm trigger storage
C!   Description :
C!   ===========
C?   Update trigger statistics
C?   IF (Itype<=2) Store single event
C?   IF (Itype>=3) Update Bhabha statistics
C?   Update histograms LEHI and LVHI
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "lolejj.h"
      EXTERNAL NAMIND
C---------------------------------------------------------------------
C
C Do trigger analysis
      CALL LTRIGS(ITYPE)
C
C Restrict to Lumi triggers
      IF(ITYPE.EQ.0)                               GOTO 999
C
C IF exclusive single arm trigger, store it
      IF(ITYPE.LE.2) CALL LSINGL(ITYPE)
C
C IF coincidence trigger or very high single, update Bhabha counters
      IF(ITYPE.GE.3) CALL LSELBH(ITYPE)
C
C Fill calibration histograms
      IF(ITYPE.EQ.5) CALL LCALIB
C
  999 CONTINUE
C
      END
#endif
