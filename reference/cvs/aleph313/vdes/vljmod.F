      INTEGER FUNCTION VLJMOD (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Last global module index in face
C - Joe Rothberg, December 1993
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VLJMOD / I  Last global module index in face
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF (JFAC .GE. 1 .AND. JFAC .LE. NSLOTS) THEN
        VLJMOD = JFAC*NVMODF
C
      ELSE
        VLJMOD =  VDERR
      ENDIF
C
      RETURN
      END
#endif
