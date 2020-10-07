      INTEGER FUNCTION VNFSLO ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Determine number of slots containing a face
C - Steve Wasserbaech, February 1994
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C
C - Input:
C   (none)
C
C - Output:
C   VNFSLO / I  Total number of slots in VDET containing a face
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
C     Local variables
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      VNFSLO = 0
      DO JFAC=1,NSLOTS
        IF (ISSFLG(JFAC) .NE. 0) VNFSLO = VNFSLO + 1
      ENDDO
C
      RETURN
      END
#endif
