      SUBROUTINE CASMP(ITR,IFND)
C***********************************************************************
C! assoc. hc and tpc mip                                               *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870911                                    *
C!  MODIFIED : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : TREX                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   itr = track number                                                *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "ecnamc.h"
#include "etckjj.h"
#include "hclujj.h"
#include "hcnamc.h"
#include "bcs.h"
#include "trexjj.h"
      DIMENSION XPT(3)
      EXTERNAL NLINK
#include "bmacro.h"
      KTREX=NLINK('TREX',ITR)
      KETCK=IW(NAETCK)
      KHCLU=IW(NAHCLU)
C
C? Get associated ecob
C
      IECOB=ITABL(KETCK,ITR,JETCEC)
C
C?  Get number of points and loop on them
C
      NPTS=LROWS(KTREX)
      DO 100 IPT=1,NPTS
C
C?   Eliminate points which are not in Hcal
C
      IRG=IABS(ITABL(KTREX,IPT,JTRERG))/1000
      IF(IRG.NE.3)GO TO 100
C
C?   Get x,y,z and make encoded storey address
C
         DO 45 KC=1,3
   45    XPT(KC)=RTABL(KTREX,IPT,JTREX3-1+KC)
         CALL HFNDTW(XPT,'ALEPH',JT,IFI,KS,ISUBC,IMOD,IPL,IDEAD)
         IF(IDEAD.NE.0)GOTO 100
C
C?   See if storey is hit
C
      CALL CISHIT(IFI,JT,KS,IHCLU)
      IF(IHCLU.NE.0)THEN
C
C? Skip noisy hclusters
C
C
C This was considered unwise, suppress it (july 1991)
C
C         NOISE=ITABL(KHCLU,IHCLU,JHCLNF)
C         IF(NOISE.EQ.0)THEN
         CALL CINSRL(ITR,IHCLU,0.,IECOB)
         IFND=1
C         ENDIF
C
C? IF storey is hit, we can exit since neighbouring storeys
C?         belong to the same cluster
C
         GO TO 100
      ELSE
C
C?     Go into cnighb, to loop on neighbouring storeys and find
C?       associated clusters
         RMS=RTABL(KTREX,IPT,JTRERM)
C
         CALL CNIGHB(XPT,RMS,JT,IFI,KS,ITR,IECOB,IFND)
      ENDIF
  100 CONTINUE
C
C? If no hclu was associated, put the track-ecob relation
C?  but only if ecob exists
C
      IF(IFND.EQ.0.AND.IECOB.NE.0)THEN
         IHCLU=0
         CALL CINSRL(ITR,IHCLU,0.,IECOB)
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
