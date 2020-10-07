      SUBROUTINE CRHCLU(ICLUH,IREHC)
C
C***********************************************************************
C!Find recons Hcal Cluster type                                        *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!  ICLUH  = Hcal cluster number                                       *
C!  IREHC = type :                                                     *
C!             1 : neutral                                             *
C!             2 : charged, mip                                        *
C!             3 : charged, nomip                                      *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HCLU,HTDT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "ccnamc.h"
#include "cct1jj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "bcs.h"
#include "bmacro.h"
      KCCT1=IW(NACCT1)
      IREL=ITABL(KCCT1,ICLUH,JCCTCR)
C
C?   say that cluster is neutral
C
      IREHC=1
      IF(IREL.NE.0)THEN
C
C?   Cluster has relations
C
         KCRL3=IW(NACRL3)
   30    ITRK=ITABL(KCRL3,IREL,JCRLCT)
         IF (ITRK.NE.0) THEN
C
C?   Cluster has relations to tracks, we assume it to be mip
C
            IREHC=2
            CALL CTRMIP(ITRK,IFMIP)
            IF (IFMIP.EQ.0) THEN
C
C?   If one track associated is not MIP, cluster is not mip
C
              IREHC=3
              GO TO 999
            ENDIF
         ENDIF
         IREL=ITABL(KCRL3,IREL,JCRLNH)
         IF(IREL.NE.0)GO TO 30
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
