      SUBROUTINE YFBOOK
C----------------------------------------------------------------------
C
C! Book histograms for Mermekides main vertex fit
C!
C!   Author: M.Mermikides    11/10/87
C!   Modified: M.Bosman 12/04/89
C
C!   Modified: M.A. Ciocci 16/4/89
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
C
C
      JOFF = JULYR * 1000
      CALL HBOOK2(JOFF+1,'X VS Y COORDINATE OF VERTEX$',50,-.5,.5,
     &                                   50,-.5,.5,0.)
      CALL HBOOK1(JOFF+2,'Z COORDINATE OF VERTEX$',50,-5.,5.,0.)
      CALL HBOOK1(JOFF + 3  ,'VERTEX FIT ERROR IN X POSITION$'
     &                 ,50, 0., 0.5,0.)
      CALL HBOOK1(JOFF + 4  ,'VERTEX FIT ERROR IN Y POSITION$'
     &                 ,50, 0., 0.5,0.)
      CALL HBOOK1(JOFF + 5  ,'VERTEX FIT ERROR IN Z POSITION$'
     &                 ,50, 0., 0.5,0.)
      CALL HBOOK1(JOFF + 6  ,'NO OF TRACKS IN PRIM. VERTEX FIT$'
     &                ,50, 0., 50.,0.)
      CALL HBOOK1(JOFF + 7  ,
     &        'PROBABILITY OF VERTEX FIT$',50,0.,1.0,0.)
C
      RETURN
      END
#endif
