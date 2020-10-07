      SUBROUTINE SRDTRK(ISTAR,NTRK,DM)
C----------------------------------------------------------------------
C! Compute distance between track cand.
C!
C!    Author:     H. Meinhard       21-Mar-1987
C!    Modified:   H. Meinhard       02-Mar-1988
C!
C!    Input:        - ISTAR       /I    number of first track candidate
C!                                      to be considered
C!                  - NTRK        /I    number of track candidates to
C!                                      be considered
C!    Output:       - DMij        /R    distance matrix
C!
C!    Description
C!    ===========
C!    For each pair of track candidates concerned, consider the
C!    geometrical distance between the tracks and divide it by
C!    its own error found by error propagation from the covariance
C!    matrices of the fits.
C?
C!======================================================================
#ifndef DOC
#include "sparam.h"
#include "bcs.h"
#include "strkjj.h"
#include "swrkbk.h"
      REAL DM(MNOSP,*)
#include "bmacro.h"
C----------------------------------------------------------------------
      DO 310 ITRK1 = 2, NTRK
        TH1 = RTABL(KWK1SW,ISTAR+ITRK1-1,JSTRTH)
        PH1 = RTABL(KWK1SW,ISTAR+ITRK1-1,JSTRPH)
        TT1 = RTABL(KWK1SW,ISTAR+ITRK1-1,JSTRTT)
        TP1 = RTABL(KWK1SW,ISTAR+ITRK1-1,JSTRTP)
        PP1 = RTABL(KWK1SW,ISTAR+ITRK1-1,JSTRPP)
C
        DO 300 ITRK2 = 1, ITRK1-1
          TH2 = RTABL(KWK1SW,ISTAR+ITRK2-1,JSTRTH)
          PH2 = RTABL(KWK1SW,ISTAR+ITRK2-1,JSTRPH)
          TT2 = RTABL(KWK1SW,ISTAR+ITRK2-1,JSTRTT)
          TP2 = RTABL(KWK1SW,ISTAR+ITRK2-1,JSTRTP)
          PP2 = RTABL(KWK1SW,ISTAR+ITRK2-1,JSTRPP)
C
          DPHI = PH2 - PH1
          CDPHI = COS(DPHI)
          SDPHI = SIN(DPHI)
          DIST2 = TH1**2 + TH2**2 - 2.*TH1*TH2*COS(DPHI)
          DIST2 = MAX(DIST2,0.)
          ROOT = TT1*(TH1-TH2*CDPHI)**2 -
     +               2.*TP1*(TH1-TH2*CDPHI)*TH1*TH2*SDPHI +
     +               PP1*(TH1*TH2*SDPHI)**2 +
     +           TT2*(TH2-TH1*CDPHI)**2 +
     +               2.*TP2*(TH2-TH1*CDPHI)*TH1*TH2*SDPHI +
     +               PP2*(TH1*TH2*SDPHI)**2
          ROOT = MAX(ROOT,0.)
          IF (ROOT .GT. 0.) THEN
            DBYSG = DIST2/SQRT(ROOT)
          ELSE IF (DIST2 .GT. 0.) THEN
            DBYSG = 1000.
          ELSE
            DBYSG = -99.
          ENDIF
          DM(ITRK1,ITRK2) = DBYSG
          DM(ITRK2,ITRK1) = DBYSG
  300   CONTINUE
  310 CONTINUE
C
      RETURN
      END
#endif
