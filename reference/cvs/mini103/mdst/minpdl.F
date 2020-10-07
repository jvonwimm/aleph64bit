      SUBROUTINE MINPDL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PDLT bank from DDLT.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#include "pdltjj.h"
#include "ddltjj.h"

C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C
C++   Pick up DDLT bank.
C
      KDDLT = NLINK('DDLT',0)
      IF (KDDLT.LE.0) RETURN
      NDDLT = LROWS(KDDLT)
      IF (NDDLT.LE.0) RETURN
C
C++   Create PDLT bank.
C
      NPDLT = NDDLT
      LEN = LMHLEN + LPDLTA * NDDLT
      CALL AUBOS('PDLT',0,LEN, KPDLT,IGARB)
      CALL BLIST(IW,'S+','PDLT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDDLT = NLINK('DDLT',0)
      ENDIF
      IW(KPDLT+LMHCOL) = LPDLTA
      IW(KPDLT+LMHROW) = NDDLT
C
C++   Fill PDLT bank.
C
      DO 100 I=1,NDDLT
         IW(KROW(KPDLT,I)+JPDLPA) = ITABL(KDDLT,I,JDDLPA)
         IW(KROW(KPDLT,I)+JPDLJT) = ITABL(KDDLT,I,JDDLJT)
         RW(KROW(KPDLT,I)+JPDLPI) = FLOAT(ITABL(KDDLT,I,JDDLPI))/AFACTM
         RW(KROW(KPDLT,I)+JPDLPE) = FLOAT(ITABL(KDDLT,I,JDDLPE))/AFACTM
         IW(KROW(KPDLT,I)+JPDLVP) = ITABL(KDDLT,I,JDDLVP)

         IW(KROW(KPDLT,I)+JPDLFR) = ITABL(KDDLT,I,JDDLFR)
  100 CONTINUE
C
      RETURN
      END
#endif
