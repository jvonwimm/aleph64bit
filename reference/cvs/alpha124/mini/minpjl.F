      SUBROUTINE MINPJL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PLJT bank from DLJT.
C
C     Author: Agnieszka Jacholkowska    14-Nov-94
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#include "pljtjj.h"
#include "dljtjj.h"

C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DLJT bank.
C
      KDLJT = NLINK('DLJT',0)
      IF (KDLJT.LE.0) RETURN
      NDLJT = LROWS(KDLJT)
      IF (NDLJT.LE.0) RETURN
C
C++   Create PLJT bank.
C
      NPLJT = NDLJT
      LEN = LMHLEN + LPLJTA * NDLJT
      CALL AUBOS('PLJT',0,LEN, KPLJT,IGARB)
      CALL BLIST(IW,'S+','PLJT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDLJT = NLINK('DLJT',0)
      ENDIF
      IW(KPLJT+LMHCOL) = LPLJTA
      IW(KPLJT+LMHROW) = NDLJT
C
C++   Fill PLJT bank.
C
      DO 100 I=1,NDLJT
         RW(KROW(KPLJT,I)+JPLJPX) = FLOAT(ITABL(KDLJT,I,JDLJPX))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPY) = FLOAT(ITABL(KDLJT,I,JDLJPY))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPZ) = FLOAT(ITABL(KDLJT,I,JDLJPZ))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPE) = FLOAT(ITABL(KDLJT,I,JDLJPE))/AFACTM
         IW(KROW(KPLJT,I)+JPLJNO) = ITABL(KDLJT,I,JDLJNO)
  100 CONTINUE
C
      RETURN
      END
#endif
