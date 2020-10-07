      SUBROUTINE ECFOBJ
C-------------------------------------------------------
C!  STEERING ROUTINE TO ANALYZE RECONSTRUCTED CLUSTERS
C!
C!  AUTHOR   : J.P. ALBANESE
C!  MODIFIED : F. ETIENNE      87/08/27
C!
C!  BANKS :
C!    INPUT   : ECLU
C!    OUTPUT  : ECOB
C!    CREATED : NONE
C!
C-------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rparac.h"
#include "rflags.h"
#include "esdajj.h"
#include "eclujj.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "ecnamc.h"
#include "bmacro.h"
C! Set up arrays of correction for storeys in overlap
C! Eovlpc is not use      CALL EOVLPC
C?   CREATE ECOB BANKS
      CALL ECOBNK(IER)
      IF (IER.NE.0) GO TO 999
C
      KECLU = IW(NAECLU)
C
      NECLU = IW(KECLU + LMHROW)
      IF (NECLU.LT.1) GO TO 99
C
C?   LOOP ON CLUSTERS
      DO 10 ICL=1,NECLU
C
C?   SKIP CLUSTER WITH NO STOREYS
      KECLU = IW(NAECLU)
      IF (ITABL(KECLU,ICL,JECLES).EQ.0) GOTO 10
C
C? Find cluster charge
C
      ICH = 0
      KECT1 = IW(NAECT1)
      KERL3=IW(NAERL3)
      ICH=0
      IREL=ITABL(KECT1,ICL,JECTER)
   11 IF (IREL.NE.0)  THEN
        ITK=ITABL(KERL3,IREL,JERLET)
        CALL ESLAS(ICL,ITK,IKEPT)
        IF(IKEPT.EQ.1)THEN
           ICH=1
           GO TO 110
        ENDIF
        IREL=ITABL(KERL3,IREL,JERLER)
        GO TO 11
      ENDIF
  110 CONTINUE
C
C
      IF (ICH.EQ.0)  THEN
C?   Analyse neutral cluster
        CALL ECNCOB(ICL,IER)
      ELSE
C?   Analyse charged cluster
        CALL ECCAOB(ICL,IER)
      ENDIF
      IF (IER.NE.0) GO TO 999
C
C? ALIGNEMENT ROUTINE
C
      IF (JHISRF(JULEC).GE.3) CALL ECALIG
   10 CONTINUE
C
C
C?   CLOSE ECOB BANKS
      CALL ECCLCO
   99 RETURN
C
  999 CALL RERROR(' ECFOBJ',1,' Error from ECCAOB')
      RETURN
      END
#endif
