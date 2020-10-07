      SUBROUTINE QV0CHK(JTRAK,CHI2,IER)
CKEY PART/USER
C----------------------------------------------------------------------
C! Get the chi2 of a track with the constraint of the primary vertex
C  ALPHA interface to the Alephlib YCHIV2 routine
C Author J. Boucrot       29-Sep-1993
C Called from USER
C Calls  YCHIV2                                 from ALEPHLIB
C  Input argument :
C  JTRAK  =  JULIA (FRFT) track number ( belonging to a V0 candidate )
C  Outputs :
C  CHI2 = chi**2 of track JTRAK with main vertex constraint
C         set to -999999. if IER = 1
C  IER  = 0 if all OK
C       = 1 if no CHI2 calculation possible
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "qcde.h"
      REAL VPRIM(3),VERMA(3,3)
      DATA EPSIL  / 1.E-20 /
#include "qmacro.h"
C----------------------------------------------------------------------
      IER = 0
      CHI2=0.
      CALL VZERO(VPRIM,3)
      CALL VZERO(VERMA,9)
      IF (KVTYPE(KFREV).NE.1) GO TO 990
C Transmit main vertex parameters to local arrays :
      VPRIM(1) = QVX(KFREV)
      VPRIM(2) = QVY(KFREV)
      VPRIM(3) = QVZ(KFREV)
C Error matrix :
      VERMA(1,1) = QVEM(KFREV,1,1)
      VERMA(1,2) = QVEM(KFREV,1,2)
      VERMA(1,3) = QVEM(KFREV,1,3)
      VERMA(2,1) = QVEM(KFREV,2,1)
      VERMA(2,2) = QVEM(KFREV,2,2)
      VERMA(2,3) = QVEM(KFREV,2,3)
      VERMA(3,1) = QVEM(KFREV,3,1)
      VERMA(3,2) = QVEM(KFREV,3,2)
      VERMA(3,3) = QVEM(KFREV,3,3)
      IF ((VERMA(1,1)+VERMA(2,2)+VERMA(3,3)).LT.EPSIL)  GO TO 990
C Compute Chi**2 :
      CHI2 = YCHIV2(JTRAK,VPRIM,VERMA,IFAIL)
      IF (IFAIL.NE.0) GO TO 990
      GO TO 999
C No calculation possible :
 990  CHI2 = -999999.
      IER=1
 999  RETURN
      END
#endif
