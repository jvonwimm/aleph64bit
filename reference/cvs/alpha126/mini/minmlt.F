      SUBROUTINE MINMLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill History of tagged lepton bank DMLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C
C     Input  : PMLT bank
C     Output : DMLT bank
C
C     Called by MINDST
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
C++   Pick up PMLT bank.
C
      KPMLT = NLINK('PMLT',0)
      IF (KPMLT.LE.0) RETURN
      NPMLT = LROWS(KPMLT)
      IF (NPMLT.LE.0) RETURN
C
C++   Create the DMLT bank.
C
      NDMLT = NPMLT
      LEN = LMHLEN + LDMLTA * NDMLT
      CALL AUBOS('DMLT',0,LEN, KDMLT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINMLT: Cannot create DMLT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPMLT = NLINK('PMLT',0)
      ENDIF
      IW(KDMLT+LMHCOL) = LDMLTA
      IW(KDMLT+LMHROW) = NDMLT
C
C++   Loop over PMLT storing information in DMLT.
C
      DO 100 I=1,NDMLT
         IW(KROW(KDMLT,I)+JDMLFL) = ITABL(KPMLT,I,JPMLFL)
         IW(KROW(KDMLT,I)+JDMLPO) = ITABL(KPMLT,I,JPMLPO)
         IW(KROW(KDMLT,I)+JDMLCH) = ITABL(KPMLT,I,JPMLCH)
         IW(KROW(KDMLT,I)+JDMLSP) = ITABL(KPMLT,I,JPMLSP)
         IW(KROW(KDMLT,I)+JDMLLE) = ITABL(KPMLT,I,JPMLLE)
         IW(KROW(KDMLT,I)+JDMLME) = ITABL(KPMLT,I,JPMLME)
         IW(KROW(KDMLT,I)+JDMLKT) = ITABL(KPMLT,I,JPMLKT)

         IW(KROW(KDMLT,I)+JDMLFR) = ITABL(KPMLT,I,JPMLFR)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DMLT')
C
      RETURN
      END
#endif
