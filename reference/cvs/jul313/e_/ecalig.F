      SUBROUTINE ECALIG
C----------------------------------------------------------------------
C!  - STEERING ROUTINE TO SELECT CANDIDATES CLUSTERS FOR ALIGNEMENT PURP
C!
C!  AUTHOR   : J.P. ALBANESE
C!
C!  BANKS :
C!    INPUT   : ECLU
C!    CREATED : NONE
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "eanclu.h"
#include "eclujj.h"
#include "etp1jj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "ecnamc.h"
      DATA EAMIN/15./
#include "bmacro.h"
C
      KECLU = IW(NAECLU)
      KCLTR = IW(NAECT1)
      KRLCT = IW(NAERL3)
      KETP1 = IW(NAETP1)
C
      NECLU = IW(KECLU + LMHROW)
      IF (NECLU.LT.1) GO TO 99
C
C?   Skip CLUSTER WITH NO STOREYS
      IF (ITABL(KECLU,ICLNEA,JECLES).EQ.0) GOTO 10
C
C?   Skip NEUTRAL CLUSTER
      IF (ICHREA.EQ.0)   GOTO 10
C?   ANALYSE CHARGED CLUSTER
C?   Select events with Energy greater than EAMIN gev
      IF (ECLTEA.LT.EAMIN) GOTO 10
C?   Select cluster on only one module
      IF (NBMOEA.GT.1  ) GOTO 10
C?   Select cluster with one associated tracks
C?   and with momentum greater than EAMIN gev
      KCTCF = KROW(KCLTR,ICLNEA)+JECTER
      NREL  = IW(KCTCF)
      KRLXX = KROW(KRLCT,NREL)
C?   Skip cluster with more than one track
      IF (IW(KRLXX+JERLER).NE.0) GOTO 10
C?   Get momentum of track
      IDTPC = IW(KRLXX+JERLET)
      KLINE = KROW(KETP1,IDTPC)
      IF (RW(KLINE+JETPPT).LT.EAMIN) GOTO 10
C?   Compute now relevant distance cluster-track
      CALL EDISAL(IDTPC)
   10 CONTINUE
C
C
   99 RETURN
C
      END
#endif
