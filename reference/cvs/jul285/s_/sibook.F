      SUBROUTINE SIBOOK
C----------------------------------------------------------------------
C!  - SICAL Histograms booking
C!
C!   Book Scal histograms
C!
C!   Author   : B.Bloch-Devaux 911029
C?   Book Sical histos in JULIA
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C----------------------------------------------------------------------
C
      LHID0= JULSI * 1000
C
C Histogram events with Scal clusters for general monitoring
      DO 10 II= 1,3
        LHID = LHID0+100*(II-1)
        CALL HBOOK1(LHID + 1,'SCAL- Pad energy sum',60,0., 90.,0.)
        CALL HBOOK1(LHID +12,'SCAL- Number of Pad /evt',30,0.,360.,0.)
        CALL HBOOK1(LHID +11,'SCAL- Number of cluster/evt', 5,0., 5.,0.)
        CALL HBOOK1(LHID +16,'SCAL- Residual Calibration',20,0.6,1.4,0.)
        CALL HBOOK2(LHID + 8,'SCAL- E Mod A vs Mod B',40,0.,60.,40,0.,
     $    60.,0.)
        CALL HBOOK2(LHID + 2,'SCAL- NPad/cluster vs Cluster energies',
     $    30,2.5,302.5,40,2.,62.,0.)
        CALL HBPRO(LHID+2,0.)
        CALL HBOOK1(LHID + 3,'SCALA Z Profile from pads',13,0.,13.,0.)
        CALL HBOOK1(LHID +23,'SCALB Z Profile from pads',13,0.,13.,0.)
        CALL HBOOK1(LHID + 4,'SCALA R Cluster ',44,5.,16.,0.)
        CALL HBOOK1(LHID +24,'SCALB R Cluster ',44,5.,16.,0.)
        CALL HBOOK1(LHID + 5,'SCALA Z Cluster ',48,250.,262.,0.)
        CALL HBOOK1(LHID +25,'SCALB Z Cluster ',48,250.,262.,0.)
        CALL HBOOK1(LHID + 6,'SCALA PHI Cluster ',32,0.,6.2832,0.)
        CALL HBOOK1(LHID +26,'SCALB PHI Cluster ',32,0.,6.2832,0.)
C Average X and Y
        CALL HBOOK2(LHID + 31,'SCAL xclu vs y A SIDE',
     *   50,-15.,15.,50,-15.,15.,0.)
        CALL HBPRO(LHID+31,0.)
        CALL HBOOK2(LHID + 51,'SCAL xclu vs y B SIDE',
     *    50,-15.,15.,50,-15.,15.,0.)
        CALL HBPRO(LHID+51,0.)
        CALL HBOOK2(LHID + 7,'SCALA E VS R clu',18,6.,15.,40,20.,60.,0.)
        CALL HBOOK2(LHID +27,'SCALB E VS R clu',18,6.,15.,40,20.,60.,0.)
        CALL HBOOK2(LHID + 9,'SCALA E VS phclu',64,0.,360.,40,20.,60.,
     &    0.)
        CALL HBOOK2(LHID +29,'SCALB E VS phclu',64,0.,360.,40,20.,60.,
     &    0.)
   10 CONTINUE
C
  999 CONTINUE
      END
#endif
