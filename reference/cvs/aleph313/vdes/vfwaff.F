      INTEGER FUNCTION VFWAFF (IWFF,IMOD,IWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local module and local wafer from wafer-in-face
C - Joe Rothberg, December 1993
C
C - Input:
C   IWFF   / I  Local wafer-in-face index
C
C - Output:
C   VFWAFF / I  = VDOK if successful
C               = VDERR if error occurred
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vgindx.h"
C
      INTEGER IWFF, IMOD, IWAF
C
C ----------------------------------------------------------------------
C
      IF (IWFF .GE. 1 .AND. IWFF .LE. NWAFEF) THEN
C
        IF (IWFF .GT. NWAFEM) THEN
          IMOD = 2
          IWAF = IWFF - NWAFEM
        ELSE
          IMOD = 1
          IWAF = 1 - IWFF + NWAFEM
        ENDIF
C
        VFWAFF = VDOK
      ELSE
        VFWAFF =  VDERR
      ENDIF
C
      RETURN
      END
#endif
