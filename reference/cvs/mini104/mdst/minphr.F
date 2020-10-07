      SUBROUTINE MINPTH
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PTHR bank from DTHR.
C
C     Author: Agnieszka Jacholkowska      19-Sep-94
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
#include "dthrjj.h"
#include "pthrjj.h"
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C
      IBNK = 0
      KDTHR = NLINK('DTHR',IBNK)
      IF(KDTHR.LE.0) GOTO 1000
C
C++   Create PTHR bank.
C
c     NPTHR = LROWS(KDTHR)
      NPTHR = 1
      IF(NPTHR.LE.0) GOTO 1000
      LEN = LMHLEN + LPTHRA * NPTHR
      CALL AUBOS('PTHR',IBNK,LEN, KPTHR,IGARB)
      CALL BLIST(IW,'S+','PTHR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTHR = NLINK('DTHR',IBNK)
      ENDIF
      IW(KPTHR+LMHCOL) = LPTHRA
      IW(KPTHR+LMHROW) = NPTHR
C
C++   Fill PTHR bank.
C
      RW(KROW(KPTHR,1)+JPTHTH) = FLOAT(ITABL(KDTHR,1,JDTHTH))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPX) = FLOAT(ITABL(KDTHR,1,JDTHPX))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPY) = FLOAT(ITABL(KDTHR,1,JDTHPY))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPZ) = FLOAT(ITABL(KDTHR,1,JDTHPZ))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPE) = FLOAT(ITABL(KDTHR,1,JDTHPE))/AFACTM

c     WRITE(IW(6),'('' MINPTH: PTHR bank lifted'')')
 1000 CONTINUE
C
      RETURN
      END
#endif
