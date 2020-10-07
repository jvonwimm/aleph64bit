      SUBROUTINE SABOOK
C----------------------------------------------------------------------
C! Book SATR histograms
C!
C!    Author:     H. Meinhard       09-Oct-1987
C!    Modified:   H. Meinhard       04-Jul-1989  (2)
C!
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "rflags.h"
#include "rparac.h"
#include "sparam.h"
      CHARACTER*51 THIST(MHISP),THIS1(MH1SP)
      DATA THIST /
     +  'SATR: NUMBER OF RAW DATA PER EVENT                $',
     +  'SATR: NUMBER OF COORDINATES PER EVENT ON SIDE A   $',
     +  'SATR: NUMBER OF COORDINATES PER EVENT ON SIDE B   $',
     +  'SATR: NUMBER OF TRACKS PER EVENT ON SIDE A        $',
     +  'SATR: NUMBER OF TRACKS PER EVENT ON SIDE B        $',
     +  'SATR: THETA DISTRIBUTION OF TRACKS ON SIDE A      $',
     +  'SATR: PI - THETA DISTRIBUTION OF TRACKS ON SIDE B $',
     +  'SATR: PHI DISTRIBUTION OF TRACKS ON SIDE A        $',
     +  'SATR: PI + PHI DISTRIBUTION OF TRACKS ON SIDE B   $',
     +  'SATR: TRACKS ON SIDE A (CENTRAL VIEW)             $',
     +  'SATR: TRACKS ON SIDE B (CENTRAL VIEW)             $',
     +  'SATR: NUMBER OF PATCH PARAM. PER EVENT ON SIDE A  $',
     +  'SATR: NUMBER OF PATCH PARAM. PER EVENT ON SIDE B  $',
     +  'SATR: THETA DISTRIBUTION OF PATCH PARAM. ON SIDE A$',
     +  'SATR: PI - THETA DIST OF PATCH PARAM. ON SIDE B   $',
     +  'SATR: PHI DISTRIBUTION OF PATCH PARAM. ON SIDE A  $',
     +  'SATR: PI + PHI DIST OF PATCH PARAM. ON SIDE B     $'/
      DATA THIS1 /
     +  'SATR: NUMBER OF HITS PER TRACK                    $',
     +  'SATR: LAYERS OF HITS IN TRACK FITS                $',
     +  'SATR: CHISQUARE PROBABILITIES OF TRACK FITS       $',
     +  'SATR: RESIDUALS OF TRACK FITS                     $',
     +  'SATR: NUMBER OF HITS PER TRACK, ONLY BEST TRACK   $',
     +  'SATR: LAYERS OF HITS IN TRACK FITS, BEST TRACK    $',
     +  'SATR: CHISQ PROB OF TRACK FITS, ONLY BEST TRACK   $',
     +  'SATR: RESIDUALS OF TRACK FITS, ONLY BEST TRACK    $',
     +  'SATR: MEASURED DISTANCES COORDINATE - WIRE        $',
     +  'SATR: DISTANCE BETWEEN COORDS IN FIT AND WIRES    $',
     +  'SATR: DISTANCE BETWEEN TRACK FIT AND WIRE         $',
     +  'SATR: DISTANCE BETWEEN COORDS IN BEST FIT AND WIRE$',
     +  'SATR: DISTANCE BETWEEN BEST TRACK FIT AND WIRES   $'/
C since the SATR geometry common is not yet filled, we have to use
C DATA statements
      DATA THMIN/.035/, THMAX/.105/
C REMAX is the maximum value of the residuals in histograms
      DATA REMAX/.1/
C DMAX is the maximum distance between coordinates and wires
      DATA DMAX/.5/
C----------------------------------------------------------------------
      LHIST = 1000 * JULSA
C
C level 1 histograms
      IF (JHISRF(JULSA) .GE. 1) THEN
        CALL HBOOK1(LHIST+MRDSP,THIST(1),100,0.,100.,0.)
        CALL HBOOK1(LHIST+MCASP,THIST(2),100,0.,100.,0.)
        CALL HBOOK1(LHIST+MCBSP,THIST(3),100,0.,100.,0.)
        CALL HBOOK1(LHIST+MTASP,THIST(4),100,0.,100.,0.)
        CALL HBOOK1(LHIST+MTBSP,THIST(5),100,0.,100.,0.)
        CALL HBOOK1(LHIST+MPCSP,THIST(12),100,1.,101.,0.)
        CALL HBOOK1(LHIST+MPDSP,THIST(13),100,1.,101.,0.)
      ENDIF
C
C level 2 histograms
      IF (JHISRF(JULSA) .GE. 2) THEN
        CALL HBOOK1(LHIST+MDASP,THIST(6),70,THMIN,THMAX,0.)
        CALL HBOOK1(LHIST+MDBSP,THIST(7),70,THMIN,THMAX,0.)
        CALL HBOOK1(LHIST+MPASP,THIST(8),100,0.,TWOPI,0.)
        CALL HBOOK1(LHIST+MPBSP,THIST(9),100,0.,TWOPI,0.)
        CALL HBOOK1(LHIST+MTCSP,THIST(14),70,THMIN,THMAX,0.)
        CALL HBOOK1(LHIST+MTDSP,THIST(15),70,THMIN,THMAX,0.)
        CALL HBOOK1(LHIST+MBCSP,THIST(16),100,0.,TWOPI,0.)
        CALL HBOOK1(LHIST+MBDSP,THIST(17),100,0.,TWOPI,0.)
        CALL HBOOK1(LHIST+MN1SP,THIS1(1),10,0.,10.,0.)
        CALL HBOOK1(LHIST+MS1SP,THIS1(2),10,0.,10.,0.)
        CALL HBOOK1(LHIST+MC1SP,THIS1(3),100,0.,1.,0.)
        CALL HBOOK1(LHIST+MR1SP,THIS1(4),100,-REMAX,+REMAX,0.)
        CALL HBOOK1(LHIST+MM1SP,THIS1(9),100,0.,DMAX,0.)
        CALL HBOOK1(LHIST+MK1SP,THIS1(10),100,-DMAX,DMAX,0.)
        CALL HBOOK1(LHIST+MF1SP,THIS1(11),100,-DMAX,DMAX,0.)
      ENDIF
C
C level 3 histograms
      IF (JHISRF(JULSA) .GE. 3) THEN
        CALL HBOOK2(LHIST+MSASP,THIST(10),100,-.1,.1,100,-.1,.1,30.,0.)
        CALL HBOOK2(LHIST+MSBSP,THIST(11),100,-.1,.1,100,-.1,.1,30.,0.)
        CALL HBOOK1(LHIST+MN2SP,THIS1(5),10,0.,10.,0.)
        CALL HBOOK1(LHIST+MS2SP,THIS1(6),10,0.,10.,0.)
        CALL HBOOK1(LHIST+MC2SP,THIS1(7),100,0.,1.,0.)
        CALL HBOOK1(LHIST+MR2SP,THIS1(8),100,-REMAX,+REMAX,0.)
        CALL HBOOK1(LHIST+MK2SP,THIS1(12),100,-DMAX,DMAX,0.)
        CALL HBOOK1(LHIST+MF2SP,THIS1(13),100,-DMAX,DMAX,0.)
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
