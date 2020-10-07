*DK itdrft
      SUBROUTINE ITDRFT(INDEX,I1,I3)
C.
C...ITDRFT  3.10  920213  14:18                         R.Beuselinck.
C.
C!  Calculate the drift time due to a set of hits in a single cell.
C.  INDEX contains pointers to the 3rd word (wire no.) of each hit
C.  ordered by increasing wire number.
C.  I1 and I3 give the first and last indices in INDEX for hits in
C.  the current cell.
C.
C.  Called by: ITDIGI                                 from this .HLB
C.      Calls: WBANK                                  from BOS77
C.             RNDM                                   from CERNLIB
C.
C.  Named banks used:
C.  IHIT - read only.
C.  Work banks used:
C.  JDITWP - created at start of each event. Filled on each call.
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "itelec.h"
#include "itnamc.h"
#include "itparc.h"
C
      EXTERNAL INTCHA, RNDM
C
      INTEGER INDEX(*),I1,I3
      REAL XB(4),YB(4)
      LOGICAL OK
      PARAMETER (SMALL = 1.E-6)
#include "bmacro.h"
C ---------------------------------------------------------------
C
C - Get indices for 'IHIT'
      JIHIT = IW(NAIHIT)
      LNHT  = LROWS(JIHIT)
      LWHT  = LCOLS(JIHIT)
      IGO   = JIHIT + LMHLEN
C
C--  Create temporary wire-pulse bank on first call for each event.
C--
      IF (JDITWP.EQ.0) THEN
        CALL WBANK(IW,JDITWP,LMHLEN+LITWP*LNHT,*998)
        IW(JDITWP+LMHCOL) = LITWP
        IW(JDITWP+LMHROW) = 0
        IW(JDITWP-3) = INTCHA('ITWP')
      ELSE
        IF (LFRWRD(JDITWP) .LT. LITWP*(I3-I1+1)) THEN
          ND = IW(JDITWP) + IW(JDITWP)/2
          CALL WBANK(IW,JDITWP,ND,*998)
        ENDIF
      ENDIF
C
C--  Get parameters of the layer needed for calculating the grid
C--  interpolation.
C--
      II = IGO + INDEX(I1) - 2
      LAY = IW(II+1)
      NW  = IW(II+2)
C
C--  Loop over all hits on the current wire.
C--
      LNWP  = IW(JDITWP+LMHROW)
      DO 100 I=I1,I3
C
C--     Compute random hardware inefficiency.  The inefficiency
C--     decreases with increasing |Cos(Theta)|.
C--
        IF (HWEFIT.GT.0.0) THEN
          TEST = RNDM(DUM1)
          EFF = 1.0 - HWEFIT*(1. - ABS(COS(RW(II+6))))
          IF (TEST.GT.EFF) GO TO 100
        ENDIF
C
        II = IGO + INDEX(I) - 2
        IDHT = INDEX(I)/LWHT + 1
        ITCUR = IW(II)
C
C--     Smear drift distance and compute drift time using
C--     interpolation table.
C--
        DHTMN = RW(II+3)
        DANG  = RW(II+4)
        CALL ITRES(LAY, DHTMN, DANG)
        CALL ITDTIM(LAY, DHTMN, TIME, OK)
        IF (.NOT.OK) GO TO 100
C
C--     Here we have the drift time for one track segment in a cell
C--     (without TOF added).  However, final digitisation comes from the
C--     first signal to arrive from all track segments.  Therefore store
C--     temporarily in induced signals bank and subsequently propagate
C--     to both ends of the wire.
C--     TOF + drift time.
C--
        TPLS = RW(II+7) + ABS(TIME)
C
C--     Check that final time does not come later than TDC stop signal.
        IF (TPLS.GT.TOFFIT) GO TO 100
        JIND = KNEXT(JDITWP)
C--                             ! Hit.ID causing pulse.
        IW(JIND+1) = IDHT
C--                             ! wire no. (redundant but useful).
        IW(JIND+2) = NW
C--                             ! Time of induced wire pulse.
        RW(JIND+3) = TPLS
        IW(JDITWP+LMHROW) = IW(JDITWP+LMHROW) + 1
  100 CONTINUE
      GO TO 9999
C
C - Handle lack of BOS space
 998  CONTINUE
      IW(1) = LITWBK
      CALL WDROP(IW,JDITWB)
      CALL ALTELL('ITDRFT: not enough space for ITxx ',1,'NEXT' )
C
 9999 CONTINUE
      END
#endif
