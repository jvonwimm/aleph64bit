      SUBROUTINE ITHIST
C-----------------------------------------------------------------------
C! ITC Histograms.
C!
C!    Author     :- J. Sedgbeer   87/11/06
C!    Modified   :- J. Sedgbeer   88/05/09
C!    Modified   :- I. Tomalin    89/02/28
C!
C!    Input:
C!      commons:       /BCS/    for ITC banks
C!                      => ITCO,ITDR,IGTL,IPJT,IRJT
C!                     /IWBNKT/ pointers to work banks for ITC tracking
C!      parameters:    ITCOJJ   parameters for ITCO bank
C!                     IGTLJJ   parameters for IGTL bank
C!                     IPJTJJ   parameters for IPJT bank
C!                     IRJTJJ   parameters for IRJT bank
C!
C!    Output:   histograms.
C!
C!    called by : RHSEVT
C!
C!    calls     : none
C!
C!    Libraries required: BOS, HBOOK
C!
C? Histograms for ITC coordinates
C?   Get ITCO bank
C?   histogram # coordinates per event
C?             coord. Phi values
C?             coord. Z values
C?   Get ITDR bank
C?
C? Loop over the two sets of histograms for ITC tracking. The first set
C? being for tracks obtained by extending the TPC ones. The second set
C? being for tracks found using only the ITC.
C? Histograms for ITC-TPC tracking
C?   Get appropriate IGTL bank (number 1 or 2)
C?   histogram # JULIA ITC tracks of this type
C?             # of coords per track
C?   Get appropriate IPJT bank (number 1 or 2)
C?   Get appropriate IRJT bank (number 1 or 2)
C?   histogram fraction of hits on each JULIA ITC track which were
C?               shared with its closest true MC track.
C?             fraction of hits on each true MC track which were
C?               shared with its closest JULIA ITC track.
C? End Loop
C?
C? Histograms for ITC tracking - MC truth information
C?   histogram # true tracks in ITC (with > 0 digitisings)
C?             # digitisings/track
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "bcs.h"
#include "bosext.h"
#include "iwbnkt.h"
#include "itcojj.h"
#include "igtljj.h"
#include "ipjtjj.h"
#include "irjtjj.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      JOFF = JULIT*1000
C
C-----------------------------------------------------------------------
C Histograms for original ITC coordinates
C
C ITCO bank
C
      IF(JHISRF(JULIT).GE.2) THEN
        KITC1=NLINK('ITCO',1)
        IF(KITC1.LE.0) THEN
          X = 0.0
          CALL HFILL(JOFF+101,X,0.,1.)
        ELSE
C
          NROW=LROWS(KITC1)
          X = FLOAT(NROW)
          CALL HFILL(JOFF+101,X,0.,1.)
C
          IF(JHISRF(JULIT).GE.3) THEN
            DO 100 I=1,NROW
              PH1 = RTABL(KITC1,I,JITCP1)
              PH2 = RTABL(KITC1,I,JITCP2)
              Z   = RTABL(KITC1,I,JITCZH)
              CALL HFILL(JOFF+102,PH1,0.,1.)
              CALL HFILL(JOFF+102,PH2,0.,1.)
              CALL HFILL(JOFF+103,  Z,0.,1.)
  100       CONTINUE
          ENDIF
        ENDIF
      END IF
C
      KITDR = NLINK('ITDR',0)
C  Find the total number of true MC tracks which caused hits in the ITC.
      IF (KITDR.NE.0) NTRTR = LROWS(KITDR)
C
C-----------------------------------------------------------------------
C Create two sets of histograms. Firstly for the JULIA ITC tracks which
C were found by extending TPC tracks and then then secondly fot those
C which were only found in the ITC.
C
      DO 300 ITYPE=1,2
        KIGTL = NLINK('IGTL',ITYPE)
        IF (KIGTL.LE.0) THEN
          CALL HFILL(JOFF+100*ITYPE+101,0.0,0.0,1.0)
        ELSE
          NTRK = LROWS(KIGTL)
          NITC = 0
          DO 200 I=1,NTRK
            II = KROW(KIGTL,I)
            NI = IW(II+JIGTN1) + IW(II+JIGTNR)
            IF (NI.GT.0) THEN
              NITC = NITC + 1
              X = FLOAT(NI)
              CALL HFILL(JOFF+100*ITYPE+102,X,0.,1.)
            ENDIF
  200     CONTINUE
          X = FLOAT(NITC)
          CALL HFILL(JOFF+100*ITYPE+101,X,0.,1.)
C
C  Now compare JULIA ITC with true MC tracks.
C
          KIPJT=NLINK('IPJT',ITYPE)
          KIRJT=NLINK('IRJT',ITYPE)
          IF (KITDR.EQ.0.OR.KIPJT.EQ.0.OR.KIRJT.EQ.0) GOTO 300
C
C  Loop over the JULIA ITC tracks.
          DO 250 I=1,NTRK
C  Find the number of hits assigned by JULIA to the track.
            NCJU = ITABL(KIPJT,I,JIPJNH)
            IF (NCJU.EQ.0) GOTO 250
C  Find the number of true MC tracks sharing hits with it.
            NTRSH = ITABL(KIPJT,I,JIPJNS)
C  Find the offset to the 1st of these tracks in the IRJT bank.
            IOFF = ITABL(KIPJT,I,JIPJRN) - 1
C  Now loop over those true MC tracks with which it shared hits to
C  find out which it resembled most closely.
            NBEST = 0
            DO 225 J=1,NTRSH
C  Find the KINE number of track "J".
              NJG = ITABL(KIRJT,IOFF+J,JIRJGN)
C  Find the number of hits which the two tracks share.
              NHTSH = ITABL(KIRJT,IOFF+J,JIRJSH)
C             WRITE(LDEBRL,210) I,NHTSH,NJG
  210         FORMAT(' ITHIST : JULIA ITC track ',I3,' shares ',
     +        I3,' hits with true MC track ',I3)
              IF (NHTSH.GT.NBEST) THEN
                NBEST = NHTSH
              END IF
  225       CONTINUE
C  Plot the fraction of the hits on the JULIA ITC track which were
C  shared with its closest real MC track.
            FRAC = FLOAT(NBEST)/FLOAT(NCJU)
C  A slight fiddle next to ensure that points with FRAC=1.0 lie inside
C  the histogram.
            IF (FRAC.GE.1.0.AND.FRAC.LE.1.001) FRAC = 0.999
            CALL HFILL(JOFF+100*ITYPE+103,FRAC,0.0,1.0)
  250     CONTINUE
        ENDIF
  300 CONTINUE
C
C-----------------------------------------------------------------------
C Histograms for ITC Monte Carlo tracks
C
      IF(JHISRF(JULIT).GE.2) THEN
        IF (KITDR.NE.0) THEN
          X      = FLOAT(NTRTR)
          CALL HFILL(JOFF+401,X,0.,1.)
          DO 400 I=1,NTRTR
C  Find  the total number of digitisings caused by this track.
            NDI  = ITABL(KITDR,I,2)
            X    = FLOAT(NDI)
            CALL HFILL(JOFF+402,X,0.,1.)
  400     CONTINUE
        END IF
      END IF
      END
#endif
