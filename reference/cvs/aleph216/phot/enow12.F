       SUBROUTINE ENOW12(XW1,XW2,ECLU,THET,ESW1N,ESW2N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input   XW1 : 1st longitudinal estimator
C!             XW2 : 2nd longitudinal estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESW1N: Normalised 1st estimator
C!             ESW2N: Normalised 2nd estimator
C!     Banks used EW1N , EW2N
C---------------------------------------------------
#ifndef DOC
      SAVE
#include "ew1njj.h"
#include "ew2njj.h"
#include "bcs.h"
       DIMENSION XPARM(3,2)
       INTEGER ALGTDB,GTSTUP
       CHARACTER DET*2, LIST*8
       DATA NAEW1N,NAEW2N /2*0/, IROLD/0/, DET/'EC'/, LIST/'EW1NEW2N'/
#include "bmacro.h"
C
C- 1st entry
      IF (NAEW1N.EQ.0) THEN
         NAEW1N = NAMIND('EW1N')
         NAEW2N = NAMIND('EW2N')
      ENDIF
C
C-     Access the normalisation bank
C
#include "gtdbbk.h"
       ESW1N = -99.
       ESW2N = -99.
       KEW1N = IW(NAEW1N)
       IF ( KEW1N.EQ.0) GO TO 999
       KEW2N = IW(NAEW2N)
C
C      Choose the parameters according theta range
C
       NEW1N = 0
       IF (KEW1N.NE.0) NEW1N = LROWS(KEW1N)
       DO IEW1N = 1,NEW1N
          JEW1N = KROW(KEW1N,IEW1N)
          IF (THET.GE.RW(JEW1N+JEW1MT).AND.THET.LT.RW(JEW1N+JEW1XT))THEN
             CALL UCOPY(RW(JEW1N+JEW1P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEW1N+JEW1S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XW1,ECLU,XPARM,ESW1N)
       NEW2N = 0
       IF (KEW2N.NE.0) NEW2N = LROWS(KEW2N)
       DO IEW2N = 1,NEW2N
          JEW2N = KROW(KEW2N,IEW2N)
          IF (THET.GE.RW(JEW2N+JEW2MT).AND.THET.LT.RW(JEW2N+JEW2XT))THEN
             CALL UCOPY(RW(JEW2N+JEW2P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEW2N+JEW2S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XW2,ECLU,XPARM,ESW2N)
 999   CONTINUE
       RETURN
       END
#endif
