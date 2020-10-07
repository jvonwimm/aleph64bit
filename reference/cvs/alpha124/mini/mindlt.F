      SUBROUTINE MINDLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Reconstructed tagged lepton bank DDLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C
C     Input  : PDLT bank
C     Output : DDLT bank
C
C     Called by MINDST
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
C-----------------------------------------------------------------------
C++   Pick up PDLT bank.
C
      KPDLT = NLINK('PDLT',0)
      IF (KPDLT.LE.0) RETURN
      NPDLT = LROWS(KPDLT)
      IF (NPDLT.LE.0) RETURN
C
C++   Create the DDLT bank.
C
      NDDLT = NPDLT
      LEN = LMHLEN + LDDLTA * NDDLT
      CALL AUBOS('DDLT',0,LEN, KDDLT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINDLT: Cannot create DDLT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPDLT = NLINK('PDLT',0)
      ENDIF
      IW(KDDLT+LMHCOL) = LDDLTA
      IW(KDDLT+LMHROW) = NDDLT
C
C++   Loop over PDLT storing information in DDLT.
C
      DO 100 I=1,NDDLT
         IW(KROW(KDDLT,I)+JDDLPA) = ITABL(KPDLT,I,JPDLPA)
         IW(KROW(KDDLT,I)+JDDLJT) = ITABL(KPDLT,I,JPDLJT)
         IW(KROW(KDDLT,I)+JDDLPI) = NINT(AFACTM * RTABL(KPDLT,I,JPDLPI))
         IW(KROW(KDDLT,I)+JDDLPE) = NINT(AFACTM * RTABL(KPDLT,I,JPDLPE))
         IW(KROW(KDDLT,I)+JDDLVP) = ITABL(KPDLT,I,JPDLVP)

         IW(KROW(KDDLT,I)+JDDLFR) = ITABL(KPDLT,I,JPDLFR)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DDLT')
C
      RETURN
      END
#endif
