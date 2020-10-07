      SUBROUTINE MINPML
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PMLT bank from DMLT.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
#include "pmltjj.h"
#include "dmltjj.h"

C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DMLT bank.
C
      KDMLT = NLINK('DMLT',0)
      IF (KDMLT.LE.0) RETURN
      NDMLT = LROWS(KDMLT)
      IF (NDMLT.LE.0) RETURN
C
C++   Create PMLT bank.
C
      NPMLT = NDMLT
      LEN = LMHLEN + LPMLTA * NDMLT
      CALL AUBOS('PMLT',0,LEN, KPMLT,IGARB)
      CALL BLIST(IW,'S+','PMLT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDMLT = NLINK('DMLT',0)
      ENDIF
      IW(KPMLT+LMHCOL) = LPMLTA
      IW(KPMLT+LMHROW) = NDMLT
C
C++   Fill PMLT bank.
C
      DO 100 I=1,NDMLT
         IW(KROW(KPMLT,I)+JPMLFL) = ITABL(KDMLT,I,JDMLFL)
         IW(KROW(KPMLT,I)+JPMLPO) = ITABL(KDMLT,I,JDMLPO)
         IW(KROW(KPMLT,I)+JPMLCH) = ITABL(KDMLT,I,JDMLCH)
         IW(KROW(KPMLT,I)+JPMLSP) = ITABL(KDMLT,I,JDMLSP)
         IW(KROW(KPMLT,I)+JPMLLE) = ITABL(KDMLT,I,JDMLLE)
         IW(KROW(KPMLT,I)+JPMLME) = ITABL(KDMLT,I,JDMLME)
         IW(KROW(KPMLT,I)+JPMLKT) = ITABL(KDMLT,I,JDMLKT)

         IW(KROW(KPMLT,I)+JPMLFR) = ITABL(KDMLT,I,JDMLFR)
  100 CONTINUE
C
      RETURN
      END
#endif
