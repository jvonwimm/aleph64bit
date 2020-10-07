      INTEGER FUNCTION VFJMOD (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  First global module index in face
C - Joe Rothberg, December 1993
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VFJMOD / I  First global module index in face
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
        VFJMOD = (JFAC-1)*NVMODF + 1
      ELSE
        VFJMOD =  VDERR
      ENDIF
C
      RETURN
      END
#endif
