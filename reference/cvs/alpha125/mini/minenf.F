      SUBROUTINE MINENF
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill energy flow bank DENF for Mini-DST.
C
C     Author: Stephen Haywood      05-Feb-91
C
C     Input  : EFOL banks
C     Output : DENF bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "efoljj.h"
#include "denfjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=3,3
C
C++   Pick up EFOL bank.
C
      KEFOL = NLINK('EFOL',IBNK)
      IF(KEFOL.LE.0) GOTO 1000
      NEFOL = LROWS(KEFOL)
      IF(NEFOL.LE.0) GOTO 1000
C
C++   Create the DENF bank.
C
      NDENF = NEFOL
      LEN = LMHLEN + LDENFA * NDENF
      CALL AUBOS('DENF',IBNK,LEN, KDENF,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINENF: Cannot create DENF bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEFOL = NLINK('EFOL',IBNK)
      ENDIF
      IW(KDENF+LMHCOL) = LDENFA
      IW(KDENF+LMHROW) = NDENF
C
C++   Loop over EFOL storing information in DENF.
C++   Mass is stored rather than energy, so that the mass hypothesis is
C++   not lost by rounding.
C
      DO 100 I=1,NEFOL
         IW(KROW(KDENF,I)+JDENPX) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPX))
         IW(KROW(KDENF,I)+JDENPY) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPY))
         IW(KROW(KDENF,I)+JDENPZ) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPZ))
         AMSQ = RTABL(KEFOL,I,JEFOEW)**2
     &     - VMOD(RW(KROW(KEFOL,I)+JEFOPX),3)**2
         IW(KROW(KDENF,I)+JDENE0) = NINT(EFACTM * SQRT(MAX(AMSQ,0.)))
         IW(KROW(KDENF,I)+JDENWE) = NINT(1000.  * RTABL(KEFOL,I,JEFOWE))
         IW(KROW(KDENF,I)+JDENTY) = ITABL(KEFOL,I,JEFOTY)
         IPECO = ITABL(KEFOL,I,JEFOLE)
         IF(IPECO.GT.0) THEN
            IW(KROW(KDENF,I)+JDENPC) = IPECO
         ELSE
            IW(KROW(KDENF,I)+JDENPC) = - ITABL(KEFOL,I,JEFOLH)
         ENDIF
         IW(KROW(KDENF,I)+JDENDT) = ITABL(KEFOL,I,JEFOLT)
         IW(KROW(KDENF,I)+JDENDJ) = ITABL(KEFOL,I,JEFOLJ)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DENF')
C
C++   Add supplmentary information for EFOL/3 (ENFLW).
C
      IF (IBNK.EQ.3) CALL MINLIS('EAUX')
C
 1000 CONTINUE
C
      RETURN
      END
#endif
