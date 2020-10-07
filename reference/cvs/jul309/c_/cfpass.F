      SUBROUTINE CFPASS(IER)
C***********************************************************************
C! Det. TPC-HC assoc. param.                                           *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1,EFET,ECOB,ETCK                                    *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "etp1jj.h"
#include "efetjj.h"
#include "bcs.h"
      DIMENSION IECO(20),ICLUH(20)
      DATA PMIP/0.500/
#include "bmacro.h"
      MXCLU=20
C
C?  LOOP ON TRACKS
C
      KETP1=IW(NAETP1)
      KEFET=IW(NAEFET)
      NTRK=LROWS(KEFET)
      DO 100 ITR=1,NTRK
C
C?  GET TRACK MOMENTUM
C
      PTR=RTABL(KETP1,ITR,JETPPT)
C
C?   Select tracks with momentum larger then pmip
C
      IF(PTR.LT.PMIP)GO TO 100
C
C    Get extrap. flag
C
      IEFL=ITABL(KEFET,ITR,JEFEEF)
      IF(IEFL.NE.1)GO TO 100
C
C?   find Ec Objects in which track has deposited energy
C
C
C?   find Hc clusters in which track has deposited energy
C
         CALL CTRTOC(ITR,IECO,NECO,ICLUH,NCLU,MXCLU,IER2)
         IF(IER2.NE.0)GO TO 998
C
C?  See if track is MIP
C
      CALL CCTRMI(ITR,IFMIP)
      IF (IFMIP.EQ.1)THEN
         CALL CFPMIP(ITR,ICLUH,NCLUH)
      ELSE
         CALL CFPNMP(ITR,IECO,NECO,ICLUH,NCLUH)
      ENDIF
  100 CONTINUE
      IER=0
      RETURN
  998 CONTINUE
      CALL RERROR('CFPASS',1,' Error from CTRTOC')
      IER=1
      RETURN
      END
#endif
