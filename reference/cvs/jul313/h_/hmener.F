      SUBROUTINE HMENER(ITRAC,ECLU)
C------------------------------------------------------------------
C!      Associates hcal energy to muon candidate tracks
C!
C!      Author:         F.Bossi -LNF- 4/88
C!
C!      Input:          ITRAC =  Track nb.
C!      Output:         ECLU  =  Associated energy
C!
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "trexjj.h"
#include "rlunit.h"
#include "rparac.h"
#include "hdebug.h"
#include "hsdajj.h"
#include "hclujj.h"
#include "muocut.h"
C
#include "bmacro.h"
C
      CTASS=COS(TETASS/RADEG)
      JTREX=NLINK('TREX',ITRAC)
      NRPT=LROWS(JTREX)
C
      XMED=0.
      YMED=0.
      ZMED=0.
      NPNT=0
      DO 60 I=1,NRPT
C               region.module
C                of the point
      IREGM1=ITABL(JTREX,I,JTRERG)
      IREGM=IREGM1/100
C                   hcal points
      IF(IREGM1.LE.2500) GO TO 60
C                expected coordin.
      XMED=XMED+RTABL(JTREX,I,JTREX3)
      YMED=YMED+RTABL(JTREX,I,JTREX3+1)
      ZMED=ZMED+RTABL(JTREX,I,JTREX3+2)
      NPNT=NPNT+1
 60   CONTINUE
      IF(NPNT.EQ.0) GO TO 999
C               average coordinates of the track
      XMED=XMED/FLOAT(NPNT)
      YMED=YMED/FLOAT(NPNT)
      ZMED=ZMED/FLOAT(NPNT)
      ROMED=SQRT(XMED*XMED+YMED*YMED+ZMED*ZMED)
      VX=XMED/ROMED
      VY=YMED/ROMED
      VZ=ZMED/ROMED
C              looks for analog signals
      NAHCLU=NAMIND('HCLU')
      JHCLU=IW(NAHCLU)
      IF (JHCLU.EQ.0) GO TO 999
      NCLU=LROWS(JHCLU)
C
      COSMX=-1.1
C              looks for nearest cluster
      DO 70 J=1,NCLU
      THETC=RTABL(JHCLU,J,JHCLTC)
      PHICL=RTABL(JHCLU,J,JHCLPC)
      SINTC=SIN(THETC)
      COSA=VX*SINTC*COS(PHICL)+VY*SINTC*SIN(PHICL)+VZ*COS(THETC)
      IF (COSA.GT.COSMX) THEN
         COSMX=COSA
         ECLU=RTABL(JHCLU,J,JHCLEC)
      ENDIF
 70   CONTINUE
C                      cut on angular distance
      IF (COSMX.LT.CTASS) THEN
         ECLU=0.
      IF(IHPRIN.GT.1)WRITE(LDEBRL,78)ITRAC
 78   FORMAT(' HMENER not associated energy  track ',I3)
      ENDIF
C
 999  CONTINUE
      RETURN
      END
#endif
