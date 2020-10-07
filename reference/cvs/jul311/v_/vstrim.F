      SUBROUTINE VSTRIM(ISTRP,MSTRP,ISTRT,ISTOP)
C----------------------------------------------------------------------
C!  - Cleanup a raw hit for the hardware vagaries of the TPD
C!
C  This routine trims off non-physical strips from a MVD cluster
C
C?
C!======================================================================
#ifndef DOC
#include "vdimpl.h"
C
C  Inputs- First 'strip' number and # of 'strips' in online cluster
C
      INTEGER ISTRP,MSTRP
C
C  Outputs- first and last real strip
C
      INTEGER ISTRT,ISTOP
C
C  Local parameters
C Number of strips read out on either side
C
      INTEGER NEDGE
      PARAMETER (NEDGE = 7)
C
C
      IF(ISTRP + NEDGE .GE. 511)THEN
C
C  Trigger strip past edge of module- this hit is really at the
C  begining of the module
C
        ISTRT = 512-ISTRP
        ISTOP = MSTRP-1
        ISTRP = -ISTRT
      ELSE IF( (ISTRP+NEDGE .LT. 255 .AND. ISTRP+MSTRP .GT. 255) .OR.
     &   ISTRP+MSTRP .GT. 511)THEN
C
C  Here, the hit stradles a TPD boundary, with the trigger strip on the
C  lower side
C
        ISTRT = 0
        ISTOP = MSTRP - MOD(ISTRP+MSTRP,256) - 2
      ELSE IF( ISTRP .LE. 255 .AND. ISTRP+NEDGE .GE. 255)THEN
C
C  Here, the hit stradles a TPD boundary, with the trigger strip on the
C  upper side
C
        ISTRT = 256-ISTRP
        ISTOP = MSTRP-1
      ELSE
C
C  Normal hit
C
        ISTRT = 0
        ISTOP = MSTRP-1
      END IF
C
C  Check
C
      IF(ISTRP+ISTOP .GT. 511)THEN
        ISTOP = 511-ISTRP
      END IF
      RETURN
      END
#endif
