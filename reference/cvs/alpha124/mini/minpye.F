      SUBROUTINE MINPYE
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PYER bank from DVER.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dverjj.h"
#include "pyerjj.h"
#include "minfac.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      KDVER = NLINK('DVER',100)
      IF (KDVER.LE.0) THEN
         CALL MINUPD('DVER')
         KDVER = NLINK('DVER',100)
         IF (KDVER.LE.0) RETURN
      ENDIF
C
C++   Determine how many main vertices should be created.
C
      NPYER = 0
      DO I=1,LROWS(KDVER)
         IF(ITABL(KDVER,I,JDVEMV).GT.0) NPYER = NPYER + 1
      ENDDO
      IF(NPYER.LE.0) RETURN
C
C++   Create PYER bank.
C
      LEN = LMHLEN + LPYERA * NPYER
      CALL AUBOS('PYER',0,LEN, KPYER,IGARB)
      CALL BLIST(IW,'S+','PYER')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDVER = NLINK('DVER',100)
      ENDIF
      IW(KPYER+LMHCOL) = LPYERA
      IW(KPYER+LMHROW) = NPYER
C
C++   Fill PYER bank from DVER.
C
      DO 100 I=1,NPYER
         IW(KROW(KPYER,I)+JPYETY) = 1
         RW(KROW(KPYER,I)+JPYEVX) = FLOAT(ITABL(KDVER,I,JDVEX0))/DFACTM
         RW(KROW(KPYER,I)+JPYEVY) = FLOAT(ITABL(KDVER,I,JDVEY0))/DFACTM
         RW(KROW(KPYER,I)+JPYEVZ) = FLOAT(ITABL(KDVER,I,JDVEZ0))/DFACTM
  100 CONTINUE
C
      RETURN
      END
#endif
