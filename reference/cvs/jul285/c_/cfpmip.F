      SUBROUTINE CFPMIP(ITR,ICLUH,NCLUH)
C***********************************************************************
C! Det. TPC-HC assoc. param.                                           *
C!  for one nomip track                                                *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870911                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
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
#include "alcons.h"
#include "rparac.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "trexjj.h"
#include "bcs.h"
      DIMENSION ICLUH(*)
      DIMENSION TXX(3,50),TRTF(4,50),IKSCK(6,50)
      EXTERNAL NLINK
#include "bmacro.h"
      KHCLU=IW(NAHCLU)
      KHSDA=IW(NAHSDA)
      KTREX=NLINK('TREX',ITR)
C
C?   fill array of coordinates
C
      NPTS=LROWS(KTREX)
      IPT=0
      DO 40 I=1,NPTS
      IRG=IABS(ITABL(KTREX,I,JTRERG))/1000
C
C? eliminate points which are not in hcal
C
      IF(IRG.EQ.3)THEN
         IPT=IPT+1
C
C?   fill (x,y,z)
C
         SUM=0.
         DO 45 KC=1,3
         XCOOR=RTABL(KTREX,I,JTREX3-1+KC)
         SUM=SUM+XCOOR**2
   45    TXX(KC,IPT)=XCOOR
C
C?   fill (r,theta,phi)
C
         R=SQRT(SUM)
         TRTF(1,IPT)=R
         TRTF(2,IPT)=ACOS(TXX(3,IPT)/R)
         PHI=ATAN2(TXX(2,IPT),TXX(1,IPT))
         IF(PHI.LT.0.)PHI=PHI+TWOPI
         TRTF(3,IPT)=PHI
         CALL HFNDTW(TXX(1,IPT),'ALEPH',IT,JF,KS,ISUBC,IMOD,IPL,IDEAD)
         IKSCK(1,IPT)=IT
         IKSCK(2,IPT)=JF
         IKSCK(3,IPT)=KS
         IKSCK(4,IPT)=ISUBC
         IKSCK(5,IPT)=IMOD
         IKSCK(6,IPT)=IDEAD
C
C?   fill (Rms)
C
         TRTF(4,IPT)=RTABL(KTREX,I,JTRERM)
      ENDIF
   40 CONTINUE
      NPTS=IPT
C
C?   loop on hcal clusters
C
      DO 10 IHC=1,NCLUH
      ICLH=ICLUH(IHC)
C
C?   loop on hcal storeys of this cluster
C
      ISTFH=ITABL(KHCLU,ICLH,JHCLFS)
      ISTH=ISTFH
C
C?   Set distance to a large value
C
      DIST=10000.
   15 CONTINUE
C
C?   Compute distance between storey and track
C
      CALL CDTHS(TXX,TRTF,IKSCK,NPTS,ISTH,DLOC)
      DIST=AMIN1(DLOC,DIST)
C
C?   stop if distance is zero
C
      IF(DIST.EQ.0.)GO TO 11
      ISTH=ITABL(KHSDA,ISTH,JHSDNS)
      IF(ISTH.NE.0)GO TO 15
   11 CONTINUE
C
C?   get track momentum
C
      PX=RTABL(KTREX,ITR,JTREP3)
      PY=RTABL(KTREX,ITR,JTREP3+1)
      PZ=RTABL(KTREX,ITR,JTREP3+2)
      P=SQRT(PX**2+PY**2+PZ**2)
C
C?   get Hclu energy
C
      EHCLU=RTABL(KHCLU,IHC,JHCLEC)
      IF(DIST.GT.0.)CALL HFILL(JULCA*1000+103,DIST,0.,1.)
   10 CONTINUE
      RETURN
      END
#endif
