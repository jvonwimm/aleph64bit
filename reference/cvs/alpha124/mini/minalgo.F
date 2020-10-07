      SUBROUTINE MINALGO
CKEY MDST/INTERNAL
C ----------------------------------------------------------------------
C Create thrust DTHR and impact parameter DBTG banks
C A. Jacholkowska Oct 1994
C Put in ALPHA 8-Jan-1999
C Called from MQUEVNT
C ----------------------------------------------------------------------
#ifndef DOC
#include "minprd.h"
#include "qcde.h"
      DIMENSION SUMRY(7)
      CHARACTER*800 ELIST,RLIST
      DIMENSION CTHR(4)
#include "qmacro.h"
C ----------------------------------------------------------------------
C Perform Thrust algorithm
      CALL QJOPTR('EF',' ')
      CALL QJTHRU (THRU,'THRUST',KRECO)
      ITHRU = KPDIR('THRUST',KRECO)
      CTHR(1) = QX(ITHRU)
      CTHR(2) = QY(ITHRU)
      CTHR(3) = QZ(ITHRU)
      CTHR(4) = QE(ITHRU)
C
C Create and Fill Mini DTHR bank if THRU gt 0.
C
      IF (THRU .GT. 0.) CALL MINTHR (THRU,CTHR)
C
C Perform the IMPACT PARAMETER TAG (VDET)
C
      CALL MDIPBT
C
C Create and fill the DBTG bank
C
      CALL MDWRIP(IRET)
C
      RETURN
      END
#endif
