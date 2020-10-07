      SUBROUTINE ERCUPI(KRUN)
C-----------------------------------------------------------------------
C!initialisation for bug fix on storeys gains corrected for missing wire
CKEY ECAL CLEAN /  JULIA USER
C  B.Bloch-Devaux October 1989
C
C  Structure : SUBROUTINE
C              External references:NAMIND(BOS77)
C                                  ECPAD(This lib )
C              Comdecks references:BCS,BMACRO
C  Input : KRUN  = Run number
C  Output : none
C  Banks  : input  -  EWRG
C
C  It builds a table of correction factors for storeys in EC barrel
C  from bank EWRG content
C  This is valid for run 4001 to 4838 ,otherwise array is set to 1.
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON/EFA/FACTOR(128,384,3),ICOR
C
#include "bcs.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
      NAEWRG = NAMIND('EWRG')
C
C     No correction to be applied
C
      ICOR = 0
      IF (KRUN.GT.4838) RETURN
      IF (KRUN.LT.4001) RETURN
C
      DO 10 I = 1,128
        DO 10 J = 1,384
          DO 10 K = 1,3
   10 FACTOR(I,J,K) = 1.
C
C   Read bank EWRG from a sequential file
C
      ICOR = 1
      LUN = 3
      CALL ECPAD(LUN,IER)
      IF ( IER.NE.0) RETURN
C     Correction  to be applied
      KEWRG = IW(NAEWRG)
      IF ( KEWRG.GT.0) THEN
        NEWRG = LROWS(KEWRG)
        IF ( NEWRG.GT.0) THEN
C
C-    Loop on EWRG
C
          DO 100 IEWRG = 1,NEWRG
            IADDR = ITABL (KEWRG,IEWRG,1)
            ITET = IBITS (IADDR,16,8)
            IPHI = IBITS (IADDR,2,9)
            ISTK = IBITS(IADDR,26,2)
            FAC = RTABL(KEWRG,IEWRG,2)
            FACTOR(ITET-50,IPHI,ISTK) = FAC
  100     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
#endif
