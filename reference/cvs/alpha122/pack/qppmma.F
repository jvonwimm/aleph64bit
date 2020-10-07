      FUNCTION QPPMMA (CINP)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! get from PART the mass of particle with name CINP
C  Called from KPCOMP
C                                                 J.Boucrot  11.04.96
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "partjj.h"
      CHARACTER*12 CINP,CNAM,CPAR
#include "bmacro.h"
C----------------------------------------------------------------------
      QPPMMA=0.
C         Does PART BANK exist ?
      JPART = IW(NAPART)
      IF (JPART.LE.0) GO TO 999
      NPART = LROWS(JPART)
      CNAM=CINP
      CALL CLTOU(CNAM)
      DO 100 JPA=1,NPART
         KPA=KROW(JPART,JPA)
         CALL ALSTIN(IW(KPA+JPARNA),3,CPAR)
         CALL CLTOU(CPAR)
         IF (CPAR.NE.CNAM) GO TO 100
         QPPMMA=RW(KPA+JQPAMA)
         GO TO 999
 100  CONTINUE
 999  RETURN
      END
*I QJ960422
#endif
