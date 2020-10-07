      INTEGER FUNCTION VDPSLC (PSTRP,IVIEW,XCOOR)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM STRIP / USER
C!  Physical strip number to local wafer coordinate.
C - Joe Rothberg, 10 February 1994
C
C      Returns local wafer coordinate,
C      given physical strip number
C
C - Input:
C   PSTRP  / R  physical strip number (floating number)
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDPSLC / I  = VDOK if successful
C               = VDERR if error occurred
C   XCOOR  / R  position in local wafer coordinates
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
      INTEGER IVIEW
      REAL PSTRP
      REAL XCOOR
C
C     Local variables
C
C     PITCH       pitch
C     NSTRP       number of strips
C     ALEDG       distance from low edge of active region to center
C     GCEDG       distance from coordinate to low edge active region
C
      REAL PITCH, ALEDG, GCEDG
      INTEGER NSTRP
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
        VDPSLC = VDERR
      ELSE
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C  z-side
          PITCH = STPITZ
          NSTRP = NZSTRP
          ALEDG = AMNSRZ
        ELSE
C  rphi-side
          PITCH = STPITP
          NSTRP = NPSTRP
          ALEDG = BMNSRP
        ENDIF
C ----------------------------------------------------------------------
        IF (PSTRP.LT. 0.5 .OR. PSTRP .GT. FLOAT(NSTRP)+0.5) THEN
          VDPSLC =  VDERR
        ELSE
C  from edge
          GCEDG = PITCH*(PSTRP - 0.5)
C  in local coordinates
          XCOOR = GCEDG + ALEDG
C
          VDPSLC  = VDOK
C ----------------------------------------------------------------------
C  valid strip number
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C ----------------------------------------------------------------------
C
      RETURN
      END
#endif
