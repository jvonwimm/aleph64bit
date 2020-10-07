*DK vdbook
      SUBROUTINE VDBOOK
C-----------------------------------------------------------------------
C! Book histograms for VDET
CKEY VDET HISTO
C!
C!  Author         F.Forti         7/08/86
C!  Modified       G.Triggiani     7/06/88
C!  Modified       P.Cattaneo      17/10/88
C!  Modified       A. Bonissent    10/05/94
C!     Bug fix : two histos with index 110 were booked. Use 111
C!
C!  Description
C!  ===========
C!  If Mini-vertex is active book HBOOK4 histograms for it and
C!  specify options
C!
C!  Called by :    ASBOOK                        from this .HLB
C!  Calls     :    HBOOK1,HIDOPT                 from HBOOK4
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
C
      IF (FHISJO(1)) THEN
C
        CALL HBOOK1(101,'VDET-Number of hits per event$',
     &               50 ,0.,50.    ,0.)
        CALL HBOOK1(102,'VDET-Number of hits per wafer$',
     &               50 ,0.,50.    ,0.)
        CALL HBOOK1(103,'VDET-Number of strips per event$',
     &               100,0.,500.   ,0.)
        CALL HBOOK1(104,'VDET-Number of zed strips per wafer$',
     &               100,0.,100.   ,0.)
        CALL HBOOK1(110,'VDET-Number of r-phi strips per wafer$',
     &               100,0.,100.   ,0.)
        CALL HBOOK1(105,'VDET-Minimum hits distance in a wafer (cm)$',
     &               50 ,0.,5.     ,0.)
        CALL HBOOK1(106,'VDET-Strips'' signals (GeV)$',
     &               100,0.,500.E-6,0.)
        CALL HBOOK1(107,'VDET-Released energy (GeV)$',
     &               100,0.,500.E-6,0.)
        CALL HBOOK1(108,'VDET-Number of zed strips/hit$',
     &               10 ,0.,10.    ,0.)
        CALL HBOOK1(109,'VDET-Number of r-phi strips/hit$',
     &               10 ,0.,10.    ,0.)
        CALL HBOOK1(111,'VDET-Fraction of energy collected$',
     &               100,0.,1.E-0,0.)
C
        DO 100 ID=101,110
          CALL HIDOPT(ID,'1EVL')
  100   CONTINUE
C
      ENDIF
  999 RETURN
      END
#endif
