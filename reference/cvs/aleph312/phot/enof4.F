       SUBROUTINE ENOF4(F4,ECLU,THET,ESF4N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input    F4 : transverse estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESF4N: Normalised  estimator
C!     Banks used EF4N
C---------------------------------------------------
#ifndef DOC
       SAVE
#include "ef4njj.h"
#include "bcs.h"
       DIMENSION XPARM(4,2)
       CHARACTER DET*2, LIST*4
       INTEGER ALGTDB,GTSTUP
       DATA NAEF4N /0/ , IROLD/0/, DET/'EC'/, LIST/'EF4N'/
#include "bmacro.h"
C
C- 1st entry
       IF (NAEF4N.EQ.0) THEN
          NAEF4N = NAMIND('EF4N')
       ENDIF
C
C-     Access the normalisation bank
C
#include "gtdbbk.h"
       ESF4N = -99.
       KEF4N = IW(NAEF4N)
       IF ( KEF4N.EQ.0) GO TO 999
C
C      Choose the parameters according theta range
C
       NEF4N = 0
       IF (KEF4N.NE.0) NEF4N = LROWS(KEF4N)
       DO IEF4N = 1,NEF4N
          JEF4N = KROW(KEF4N,IEF4N)
          IF (THET.GE.RW(JEF4N+JEF4MT).AND.THET.LT.RW(JEF4N+JEF4XT))THEN
             CALL UCOPY(RW(JEF4N+JEF4P1),XPARM(1,1),4)
             CALL UCOPY(RW(JEF4N+JEF4S1),XPARM(1,2),4)
          ENDIF
       ENDDO
C
C      Calculate expected value and sigma
C
       EXP = XPARM(3,1)*((XPARM(1,1)+XPARM(2,1)/SQRT(ECLU))/XPARM(4,1))
       SEXP= XPARM(3,2)*((XPARM(1,2)+XPARM(2,2)/SQRT(ECLU))/XPARM(4,2))
       ESF4N = (F4-EXP)/SEXP
 999   RETURN
       END
#endif
