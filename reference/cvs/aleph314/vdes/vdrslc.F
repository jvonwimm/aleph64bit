      INTEGER FUNCTION VDRSLC (RSTRP,IVIEW,XCOOR)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM STRIP / USER
C!  Readout strip number to local wafer coordinate.
C - Joe Rothberg, 11 February 1994
C
C     Returns local wafer coordinate,
C     given readout strip number and view
C
C - Input:
C   RSTRP  / R  readout strip number (floating number)
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDRSLC / I  = VDOK if successful
C               = VDERR if an error occurs
C   XCOOR  / R  position in local wafer coordinates
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
C     Arguments:
      REAL RSTRP, XCOOR
      INTEGER IVIEW
C
C     Local variables
C     PSTRP    physical strip number
C
      REAL PSTRP
      INTEGER STATUS
C
C  functions
      INTEGER VDRSPS, VDPSLC
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
        VDRSLC = VDERR
      ELSE
C ----------------------------------------------------------------------
C find physical strip number
        STATUS = VDRSPS(RSTRP, IVIEW, PSTRP)
C find local coordinate
        IF (STATUS .EQ. VDOK) THEN
          STATUS = VDPSLC(PSTRP, IVIEW, XCOOR)
          IF (STATUS .EQ. VDOK) THEN
            VDRSLC  = VDOK
          ELSE
            VDRSLC = VDERR
          ENDIF
        ELSE
          VDRSLC = VDERR
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C ----------------------------------------------------------------------
      RETURN
      END
#endif
