      SUBROUTINE CTRMIP(ITR,IFMIP)
C***********************************************************************
C!Find if track is MIP particle    (recons)                            *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870505                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ecob,etck                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C?       Say that track is MIP                                         *
C?         get ecob associated with track itr                          *
C?   loop :  on hypetheses for this ecob                               *
C?         if hypothesis is mip then                                   *
C?                track is mip; exit                                   *
C?             endif                                                   *
C?         endif                                                       *
C?    endloop                                                          *
C?         if all hypotheses have been scanned, and mip was not found, *
C?         track is not MIP                                            *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "ecnamc.h"
#include "bcs.h"
#include "ehypjj.h"
#include "ecobjj.h"
#include "etckjj.h"
#include "bmacro.h"
      KETCK=IW(NAETCK)
      KECOB=IW(NAECOB)
      KEHYP=IW(NAEHYP)
      IFMIP=1
      IECOB=ITABL(KETCK,ITR,JETCEC)
      IF(IECOB.EQ.0)GO TO 999
      IFHYP=ITABL(KECOB,IECOB,JECOEH)
      IHYP=IFHYP
   10 IF(IHYP.EQ.0)GO TO 20
      ITYP=ITABL(KEHYP,IHYP,JEHYTY)
      IF(ITYP.EQ.1)GO TO 999
      IHYP=ITABL(KEHYP,IHYP,JEHYEH)
      GO TO 10
   20 CONTINUE
      IFMIP=0
  999 CONTINUE
      RETURN
      END
#endif
