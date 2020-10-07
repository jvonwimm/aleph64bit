      SUBROUTINE CGRUNC
C
C***********************************************************************
C! Get run conditions for calrec                                       *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870505                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : COPT                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
#include "crunp.h"
#include "rparac.h"
#include "rflags.h"
#include "cthres.h"
#include "coptjj.h"
#include "bcs.h"
      KCOPT=MDARD(IW,LRCONS,'COPT',0)
      IF(KCOPT.EQ.0)THEN
C
C  if the data card is missing, put default values
C   jcalpr = 0 : do not make any tpc-ecal-hcal association
C
         JCALPR=0
      ELSE
         KCOPT=KCOPT+2
         JCALPR=IW(KCOPT+JCOPOC)
         PTLOCT=RW(KCOPT+JCOPPL)
         PTHICT=RW(KCOPT+JCOPPH)
         EHLICT=RW(KCOPT+JCOPEL)
         PNMICT=RW(KCOPT+JCOPPM)
         DLIMCT=RW(KCOPT+JCOPDL)
         EDMUCT=RW(KCOPT+JCOPMU)
         IF(JHISRF(JULCA).LT.1)JHISRF(JULCA)=1
      ENDIF
      RETURN
      END
#endif
