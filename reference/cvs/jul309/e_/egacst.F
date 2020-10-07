      SUBROUTINE EGACST (IER)
C=================================================================
C!    Initialisation banks used in photon estimator normalisation
C!
C!     Author                             M.N Minard 10/02/92
C!     Modified: F.Ranjard 06/10/92
C!               call GTDBBK to get MC or Run banks
C!
C!    Output :  IER     /I    0 : all banks loaded
C!                            1 : some banks missing
C!
C!    Banks used EF4N , EW1N , EW2N , EL1N , EL2N , EGTE
C?
C!======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*24
      SAVE NAEGTE, NAEF4N, NAEW1N, NAEW2N, NAEL1N, NAEL2N
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGTEEF4NEW1NEW2NEL1NEL2N' /
      DATA NAEGTE / 0 /
      DATA IROLD  / 0 /
#include "bmacro.h"
      IF (NAEGTE.EQ.0) THEN
        NAEGTE = NAMIND('EGTE')
        NAEF4N = NAMIND('EF4N')
        NAEW1N = NAMIND('EW1N')
        NAEW2N = NAMIND('EW2N')
        NAEL1N = NAMIND('EL1N')
        NAEL2N = NAMIND('EL2N')
      ENDIF
C
#include "gtdbbk.h"
      KEGTE = IW(NAEGTE)
      KEF4N = IW(NAEF4N)
      KEW1N = IW(NAEW1N)
      KEW2N = IW(NAEW2N)
      KEL1N = IW(NAEL1N)
      KEL2N = IW(NAEL2N)
C
      IER = 0
      IF ( KEGTE.EQ.0 .OR. KEF4N.EQ.0 .OR. KEW1N.EQ.0 .OR. KEW2N.EQ.0
     &.OR. KEL1N.EQ.0 .OR. KEL2N.EQ.0 ) IER = 1
      RETURN
      END
#endif
