      SUBROUTINE MINEFO
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EFOL bank from DENF and DENW.
C
C     Author: Stephen Haywood      05-Feb-91
C     Modify: Agnieszka Jacholkowska   20-Oct-98
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "denfjj.h"
#include "denwjj.h"
#include "efoljj.h"
#include "minfac.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Need Mini version number later.
C
      MVER = MINGTV(DUM)
C
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=0,3
C
      KDENF = NLINK('DENF',IBNK)
      IF(KDENF.LE.0) GOTO 1000
C
      KDENW = NLINK('DENW',IBNK)
C
C++   Create EFOL bank.
C
      NEFOL = LROWS(KDENF)
      IF(NEFOL.LE.0) GOTO 1000
      LEN = LMHLEN + LEFOLA * NEFOL
      CALL AUBOS('EFOL',IBNK,LEN, KEFOL,IGARB)
      CALL BLIST(IW,'S+','EFOL')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDENF = NLINK('DENF',IBNK)
      ENDIF
      IW(KEFOL+LMHCOL) = LEFOLA
      IW(KEFOL+LMHROW) = NEFOL
C
C++   Fill EFOL bank.
C++   The change arising from the storing of mass rather than energy
C++   is handled here rather than in MINUPD due to various bank numbers.
C
      DO 100 I=1,NEFOL
         RW(KROW(KEFOL,I)+JEFOPX) = FLOAT(ITABL(KDENF,I,JDENPX))/EFACTM
         RW(KROW(KEFOL,I)+JEFOPY) = FLOAT(ITABL(KDENF,I,JDENPY))/EFACTM
         RW(KROW(KEFOL,I)+JEFOPZ) = FLOAT(ITABL(KDENF,I,JDENPZ))/EFACTM
         IF (MVER.GE.90) THEN
            AM = FLOAT(ITABL(KDENF,I,JDENE0))/EFACTM
            ESQ = AM**2 + VMOD(RW(KROW(KEFOL,I)+JEFOPX),3)**2
            RW(KROW(KEFOL,I)+JEFOEW) = SQRT(ESQ)
         ELSE
            RW(KROW(KEFOL,I)+JEFOEW)=FLOAT(ITABL(KDENF,I,JDENE0))/EFACTM
         ENDIF
         RW(KROW(KEFOL,I)+JEFOWE) = FLOAT(ITABL(KDENF,I,JDENWE))/1000.
         IW(KROW(KEFOL,I)+JEFOTY) = ITABL(KDENF,I,JDENTY)
         ICALO = ITABL(KDENF,I,JDENPC)
         IF(ICALO.GT.0) THEN
            IW(KROW(KEFOL,I)+JEFOLE) = + ICALO
         ELSE
            IW(KROW(KEFOL,I)+JEFOLH) = - ICALO
         ENDIF
         IW(KROW(KEFOL,I)+JEFOLT) = ITABL(KDENF,I,JDENDT)
         IW(KROW(KEFOL,I)+JEFOLJ) = ITABL(KDENF,I,JDENDJ)
C
         IF(KDENW.GT.0) THEN
           IW(KROW(KEFOL,I)+JEFOLC) = ITABL(KDENW,I,JDENLC)
         ENDIF
  100 CONTINUE
C
 1000 CONTINUE
C
      RETURN
      END
#endif
