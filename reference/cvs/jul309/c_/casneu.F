      SUBROUTINE CASNEU
C***********************************************************************
C!assoc. neutral ec and                                                *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  880315                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECOB,HCLU                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "hclujj.h"
#include "ecobjj.h"
#include "ehypjj.h"
      DIMENSION V1(4)
#include "bmacro.h"
      KECOB=IW(NAECOB)
      KHCLU=NLINK('HCLU',0)
      NHCLU=LROWS(KHCLU)
      KEHYP = NLINK('EHYP',0)
C
C?  Loop on Ecobs
C
      NECOB=LROWS(KECOB)
      DO 10 IEC=1,NECOB
C
C   See if ecob is neutral
C
      IF(ITABL(KECOB,IEC,JECOET).NE.0)GO TO 10
C
C   See if ecob is gamma
C
      CALL CISGAM(IEC,IFGAM)
      IF(IFGAM.NE.0)GO TO 10
C
C?  Fill momentum of assumed incident track
C
      SUM=0.
      IHYP = ITABL(KECOB,IEC,JECOEH)
      THYP = RTABL(KEHYP,IHYP,JEHYTH)
      PHYP = RTABL(KEHYP,IHYP,JEHYPH)
      V1(1) = SIN(THYP)*COS(PHYP)
      V1(2) = SIN(THYP)*SIN(PHYP)
      V1(3) = COS(THYP)
      V1(4)=1.
C
C?  Loop on Hcal clusters
C
      DO 110 IHCLU=1,NHCLU
C
C
C?  Make distance between ecob and hclu
C
         CALL CLUDIS(V1,IEC,IHCLU,DIST)
         IF(DIST.GE.0.)THEN
C
C?   get energy of hcal cluster
C
            EHCLU=RTABL(KHCLU,IHCLU,JHCLEC)
            PT=EHCLU*SIN(DIST)
            CALL CCUTPE(DIST,PT,EHCLU,IASSO)
            ITR=0
            IF (IASSO.EQ.1) CALL CINSRL(ITR,IHCLU,PT,IEC)
         ENDIF
  110    CONTINUE
   10 CONTINUE
      RETURN
      END
#endif
