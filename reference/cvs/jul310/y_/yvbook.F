      SUBROUTINE YVBOOK
C----------------------------------------------------------------------
C
C! Book histograms for V0 analysis
C!
C!   Author: M.A.Ciocci,L.Rolandi 14/4/88
C!   Modified: M.Bosman 12/04/89
C!   Modified: M.A. Ciocci 16/4/89
C!   Modified: M.A. Ciocci 20/7/89
C!   Modified: M.A. Ciocci 6/6/90
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
C
C Define first the histogram offset and check the histogrammingf
C level that is requested
C JOFF = JULYR * 1000 if V0 is called with Mermekides main vertex
C JOFF = JULYT * 1000 if V0 is called with the topology package
C                     or if both Mermekides and topology are called
      IHISLV = 0
C
      IF(FDETRF(JULYR)) THEN
        JOFF = JULYR * 1000
        IHISLV = JHISRF(JULYR)
      ENDIF
C
      IF(FDETRF(JULYT)) THEN
        JOFF = JULYT * 1000
        IHISLV = JHISRF(JULYT)
      ENDIF
C
            IF(IHISLV.GE.2)THEN
      CALL HBOOK1(JOFF+100,'V0 VERTEX FIT CHI SQUARE FIRST PERFORMANCE$'
     &                ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+101,'V0 VERTEX FIT CHI SQUARE SECON PERFORMANCE$'
     &                ,60,0.,1200.,0.)
      CALL HBOOK1(JOFF+102,'Q VALUE IN THE K0 IPOTHESIS$'
     &                ,100,-0.4,0.4,0.)
      CALL HBOOK1(JOFF+103,'Q VALUE IN THE LAMBDA IPOTHESIS$'
     &                ,100,-0.04,0.04,0.)
      CALL HBOOK1(JOFF+104,'Q VALUE IN THE GAMMA IPOTHESIS$'
     &                ,100,0.,0.01,0.)
            ENDIF
      RETURN
      END
#endif
