      SUBROUTINE CVRIFY
C
C***********************************************************************
C!Verify hcal-tpc association                                          *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : XRL3                                                   *
C!    OUTPUT  : XRL3                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "rparac.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "bmacro.h"
      KHCLU=IW(NAHCLU)
      NCLUH=LROWS(KHCLU)
C
C?  loop on hcal clusters
C
      DO 100 ICLUH=1,NCLUH
C
C? determine true type of the cluster
C
      CALL CTHCLU(ICLUH,ITYHC,IER)
      IF(IER.NE.0)GO TO 100
      X=ITYHC
C
C? determine recons type of the cluster
C
      CALL CRHCLU(ICLUH,ITYHC)
      YN=(ITYHC-1)*3+1
      YE=(ITYHC-1)*3+2
      CALL HFILL(JULCA*1000+103,X,YN,1.)
      EHCLU=RTABL(KHCLU,ICLUH,JHCLEC)
      CALL HFILL(JULCA*1000+103,X,YE,EHCLU)
  100 CONTINUE
      RETURN
      END

#endif
