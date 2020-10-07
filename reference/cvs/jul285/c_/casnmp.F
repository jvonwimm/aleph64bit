      SUBROUTINE CASNMP(ITR,IFND)
C***********************************************************************
C!assoc. hc, ec and tpc nomip                                          *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870911                                    *
C!  MODIFIED : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETCK,HCLU                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   itr = track number                                                *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "hclujj.h"
#include "etckjj.h"
#include "etp1jj.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "bcs.h"
      DIMENSION V1(4)
#include "bmacro.h"
      KHCLU=IW(NAHCLU)
      KETCK=IW(NAETCK)
      NHCLU=LROWS(KHCLU)
      KETP1=IW(NAETP1)
C
C?   fill vector v1 with direction of incident track
C
      DO 300 I=1,3
  300 V1(I)=RTABL(KETP1,ITR,JETPP3+I-1)
      V1(4)=1.
C
C?   Get Ecob associated to track itr
C
      IECOB=ITABL(KETCK,ITR,JETCEC)
C         WRITE(6,*)'  IECOB :',IECOB
C
C? If no Ecob associated, return
C
      IF(IECOB.EQ.0)GO TO 999
C
C
C?      Loop on Hcal clusters
C
         DO 110 IHCLU=1,NHCLU
C
C? Skip noisy hclusters
C
C
C This was considered unwise, suppress it (july 1991)
C
C         NOISE=ITABL(KHCLU,IHCLU,JHCLNF)
C         IF(NOISE.NE.0)GO TO 110
C
C?  Make distance between ecob and hclu
C
         CALL CLUDIS(V1,IECOB,IHCLU,DIST)
         IF(DIST.GE.0.)THEN
C
C?   get energy of hcal cluster
C
            EHCLU=RTABL(KHCLU,IHCLU,JHCLEC)
            PT=EHCLU*SIN(DIST)
            CALL CCUTPE(DIST,PT,EHCLU,IASSO)
            IF(IASSO.EQ.1)THEN
               CALL CINSRL(ITR,IHCLU,PT,IECOB)
               IFND=1
            ENDIF
         ENDIF
  110    CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
