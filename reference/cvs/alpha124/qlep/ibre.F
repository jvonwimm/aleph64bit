      INTEGER FUNCTION IBRE(MCT)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Handles bremsstrahlung in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from QTRUTH
C . Handles bremsstrahlung. Gives the index of the track before      .
C . bremsstrahlung. It returns 0 if this track (or some intermediate .
C . one) has no mother.                                              .
C ....................................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - variables
      INTEGER MCT,MOM
C
C - macros
#include "qmacro.h"
C----------------------------------------------------------------------
C - start
      IBRE=0
      IF (KNMOTH(MCT).EQ.0) RETURN         ! no mother
      MOM=KMOTH(MCT,1)
46    IF (CQTPN(MOM).EQ.CQTPN(MCT)) THEN
        IF (KNMOTH(MOM).EQ.0) RETURN       ! no mother
        MCT=MOM
        MOM=KMOTH(MOM,1)
        GOTO 46
      ENDIF
      IBRE=MCT
      RETURN
      END
#endif
