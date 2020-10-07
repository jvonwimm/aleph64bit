      INTEGER FUNCTION VDLCRS (XCOOR,IVIEW,RSTRP)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM STRIP / USER
C!  Local coordinate to readout strip.
C - Joe Rothberg, 11 February 1994
C     Returns readout strip number,
C     given position in local wafer coordinates, and view number.
C
C - Input:
C   XCOOR  / R  position in local wafer coordinates
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDLCRS / I  = VDOK if successful
C               = VDERR if error occurred
C   RSTRP  / R  readout strip number (floating number)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
      INTEGER IVIEW
      REAL XCOOR, RSTRP
C
C     Local variables
C
C     PSTRP       physical strip number
C
      REAL PSTRP
      INTEGER STATUS
C
C functions
      INTEGER VDLCPS, VDPSRS
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW. NE. VVIEWP)) THEN
        RSTRP = 0.
        VDLCRS = VDERR
      ELSE
C
C ----------------------------------------------------------------------
C find physical strip number
        STATUS = VDLCPS(XCOOR,IVIEW,PSTRP)
C find readout strip number
        IF (STATUS .EQ. VDOK) THEN
          STATUS = VDPSRS(PSTRP,IVIEW,RSTRP)
          IF (STATUS .EQ. VDOK) THEN
            VDLCRS  = VDOK
          ELSE
            VDLCRS = VDERR
          ENDIF
        ELSE
          VDLCRS = VDERR
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C
      RETURN
      END
#endif
