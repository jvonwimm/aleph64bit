      SUBROUTINE CHCLU(IHCLU,ICHYP,ERES,IRES,IER)
C***********************************************************************
C! Compute residual energy in hclu                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  881216                                    *
C!  Modified : A. BONISSENT  881216                                    *
C!               Skip noisy clusters                                   *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HCLU                                                   *
C!    OUTPUT  : CPAR (via cparad)                                      *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Ihclu  = hcluject number (input)                                  *
C!   Ichyp  = CalHyp number   (input)                                  *
C!   Eres(4)= Residual energy (output)                                 *
C!   Ires   = 0 : there was no residual energy; # 0 else               *
C!   Ier    = Return code (0=OK)                                       *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hclujj.h"
#include "hcnamc.h"
      DIMENSION ERES(*)
#include "bmacro.h"
      KHCLU=IW(NAHCLU)
      IRES=0
C
C? Skip noisy hclusters
C
      NOISE=ITABL(KHCLU,IHCLU,JHCLNF)
      IF(NOISE.EQ.0)THEN
         THETA=RTABL(KHCLU,IHCLU,JHCLTC)
         PHI=RTABL(KHCLU,IHCLU,JHCLPC)
         ET=RTABL(KHCLU,IHCLU,JHCLEC)
         R=ET*SIN(THETA)
         ERES(1)=R*COS(PHI)
         ERES(2)=R*SIN(PHI)
         ERES(3)=ET*COS(THETA)
         ERES(4)=ET
         IRES=1
      ENDIF
      RETURN
      END
#endif
