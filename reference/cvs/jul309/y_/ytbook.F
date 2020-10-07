      SUBROUTINE YTBOOK
C
C----------------------------------------------------------*
C!    histogram booking for the TOPOLOGY reconstruction
C!
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!
C!
C!    Description
C!    ===========
C!    This routine books histograms
C!    for the TOPOLOGY reconstruction program
C!
C!---------------------------------------------------------*
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rparac.h"
C!---------------------------------------------------------*
#include "bmacro.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IER = 0
      JOFF = JULYT * 1000
      CALL HBOOK1(JOFF,
     +      ' NB OF TRACKS IN THE PRIMARY VERTEX',50,0.,100.,0.)
      CALL HBOOK1(JOFF+1,
     +      ' X COORDINATE OF PRIMARY VERTEX',50,-.25,.25,0.)
      CALL HBOOK1(JOFF+2,
     +      ' Y COORDINATE OF PRIMARY VERTEX',50,-.25,.25,0.)
      CALL HBOOK1(JOFF+3,
     +      ' Z COORDINATE OF PRIMARY VERTEX',50,-5.,5.,0.)
      CALL HBOOK1(JOFF+4,
     +      ' CHIS2 PER DEGREE OF FREEDOM',50,0.,10.,0.)
      RETURN
      END
#endif
