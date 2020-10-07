      SUBROUTINE MINLOL
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill LOLE bank from DEVT or DHEA.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "devtjj.h"
#include "dheajj.h"
#include "lolejj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      KDEVT = NLINK('DEVT',0)
      KDHEA = NLINK('DHEA',0)
      IF (KDEVT.LE.0 .AND. KDHEA.LE.0) RETURN
C
C++   Create LOLE bank.
C
      LEN = LMHLEN + LLOLEA
      CALL AUBOS('LOLE',0,LEN, KLOLE,IGARB)
      CALL BLIST(IW,'S+','LOLE')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDEVT = NLINK('DEVT',0)
         KDHEA = NLINK('DHEA',0)
      ENDIF
      IW(KLOLE+LMHCOL) = LLOLEA
      IW(KLOLE+LMHROW) = 1
C
      IF (KDEVT.GT.0) THEN
         IER = ITABL(KDEVT,1,JDEVLE)
      ELSE
         IER = ITABL(KDHEA,1,JDHERS)
      ENDIF
      IW(KROW(KLOLE,1)+JLOLER) = IER
C
      RETURN
      END
#endif
