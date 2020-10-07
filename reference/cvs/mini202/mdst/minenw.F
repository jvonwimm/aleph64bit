      SUBROUTINE MINENW
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill energy flow bank DENW for Mini-DST.
C
C     Author: Agnieszka Jacholkowska     20-Oct-98
C
C     Input  : EFOL banks
C     Output : DENW bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "efoljj.h"
#include "denwjj.h"
#include "bmacro.h"
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
C++   Create the DENW bank.
C
      NDENW = NEFOL
      LEN = LMHLEN + LDENWA * NDENW
      CALL AUBOS('DENW',IBNK,LEN, KDENW,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINENF: Cannot create DENW bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEFOL = NLINK('EFOL',IBNK)
      ENDIF
      IW(KDENW+LMHCOL) = LDENWA
      IW(KDENW+LMHROW) = NDENW
C
C++   Loop over EFOL storing information in DENW.
C++   Mass is stored rather than energy, so that the mass hypothesis is
C++   not lost by rounding.
C
      DO 100 I=1,NEFOL
         IW(KROW(KDENW,I)+JDENLC) = ITABL(KEFOL,I,JEFOLC)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DENW')
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
