      SUBROUTINE CFPNMP(ITR,IECO,NECO,ICLUH,NCLUH)
C***********************************************************************
C! Det. TPC-HC assoc. param.                                           *
C!  for one nomip track                                                *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETCK,HCLU                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Itr   = track number                                              *
C!   Ieco  = list of ecobjects in which track has deposited energy     *
C!   Neco  = number of ecob                                            *
C!   Icluh = list of Hclusters in which track has deposited energy     *
C!   Ncluh = number of hclu                                            *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rparac.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "etp1jj.h"
#include "etckjj.h"
#include "bcs.h"
      DIMENSION IECO(*),ICLUH(*)
      DIMENSION V1(4)
#include "bmacro.h"
      KETCK=IW(NAETCK)
      KHCLU=IW(NAHCLU)
      KETP1=IW(NAETP1)
      IF(NECO.NE.0.AND.NCLUH.NE.0)THEN
         WRITE(IW(6),*)' TRACE ',ITR
         WRITE(IW(6),*)'  ECOB :',(IECO(IP),IP=1,NECO)
         WRITE(IW(6),*)'  CLUSTERS HCAL :',(ICLUH(IP),IP=1,NCLUH)
C
C?   fill vector v1 with direction of incident track
C
         DO 300 I=1,3
  300    V1(I)=RTABL(KETP1,ITR,JETPP3+I-1)
         V1(4)=1.
C
C?  get Ecob experimentally associated to track
C
         IECEX=ITABL(KETCK,ITR,JETCEC)
C
C?   loop on ecobjects really associated
C
         DO 130 IEC=1,NECO
         ICLE=IECO(IEC)
C
C?   Check that Ecobject icle is identical with iecex
C          if not, go to next track
C
         IF(ICLE.EQ.IECEX)THEN
C
C?  Get total number of hacl clusters
C        and loop on them
C
            NHCLU=LROWS(KHCLU)
            DO 111 IGHC=1,NHCLU
C
C?   Say that we shall fill the distance in the wrong associations
C?    loop on all hcal clusters
C
            NHIS=102
            DO 110 IHC=1,NCLUH
            ICLH=ICLUH(IHC)
C
C?   if the current cluster is one of the list, the histo for the good o
C
            IF(ICLH.EQ.IGHC)NHIS=101
  110       CONTINUE
            CALL CLUDIS(V1,ICLE,IGHC,DIST)
            IF(DIST.GT.-900.)THEN
               EHAD=RTABL(KHCLU,IGHC,JHCLEC)
               PT=EHAD*SIN(DIST)
               CALL HFILL(JULCA*1000+NHIS,EHAD,PT,1.)
            ENDIF
            WRITE(IW(6),*)' ITR,ICLE,ICLH,DIST,NHIS:',
     +             ITR,ICLE,ICLH,DIST,NHIS
  111       CONTINUE
         ENDIF
  130    CONTINUE
      ENDIF
      RETURN
      END
#endif
