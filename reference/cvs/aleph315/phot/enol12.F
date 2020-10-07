       SUBROUTINE ENOL12(XL1,XL2,ECLU,THET,ESL1N,ESL2N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input   XL1 : 1st longitudinal estimator
C!             XL2 : 2nd longitudinal estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESL1N: Normalised 1st estimator
C!             ESL2N: Normalised 2nd estimator
C!     Banks used EL1N , EL2N
C---------------------------------------------------
#ifndef DOC
      SAVE
#include "el1njj.h"
#include "el2njj.h"
#include "bcs.h"
       DIMENSION XPARM(3,2)
       CHARACTER DET*2, LIST*8
       INTEGER ALGTDB,GTSTUP
       DATA NAEL1N,NAEL2N /2*0/, IROLD/0/, DET/'EC'/, LIST/'EL1NEL2N'/
#include "bmacro.h"
C
C - 1st entry
      IF (NAEL1N.EQ.0) THEN
         NAEL1N = NAMIND('EL1N')
         NAEL2N = NAMIND('EL2N')
      ENDIF
C
C-     Access the normalisation bank
C
#include "gtdbbk.h"
       ESL1N = -99.
       ESL2N = -99.
       KEL1N = IW(NAEL1N)
       IF ( KEL1N.EQ.0) GO TO 999
       KEL2N = IW(NAEL2N)
C
C      Choose the parameters according theta range
C
       NEL1N = 0
       IF (KEL1N.NE.0) NEL1N = LROWS(KEL1N)
       DO IEL1N = 1,NEL1N
          JEL1N = KROW(KEL1N,IEL1N)
          IF (THET.GE.RW(JEL1N+JEL1MT).AND.THET.LT.RW(JEL1N+JEL1XT))THEN
             CALL UCOPY(RW(JEL1N+JEL1P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEL1N+JEL1S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XL1,ECLU,XPARM,ESL1N)
       NEL2N = 0
       IF (KEL2N.NE.0) NEL2N = LROWS(KEL2N)
       DO IEL2N = 1,NEL2N
          JEL2N = KROW(KEL2N,IEL2N)
          IF (THET.GE.RW(JEL2N+JEL2MT).AND.THET.LT.RW(JEL2N+JEL2XT))THEN
             CALL UCOPY(RW(JEL2N+JEL2P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEL2N+JEL2S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XL2,ECLU,XPARM,ESL2N)
 999   CONTINUE
       RETURN
       END
#endif
