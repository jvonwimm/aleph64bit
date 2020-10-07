      INTEGER FUNCTION VDRSPS (RSTRP,IVIEW,PSTRP)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Readout strip number to physical strip number.
C - Joe Rothberg, 10 February 1994
C      Returns physical strip number,
C      given readout strip number and view
C
C - Input:
C   RSTRP  / R  readout strip (floating number)
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDRSPS / I  = VDOK if successful
C               = VDERR if error occurred
C   PSTRP  / R  physical strip number (floating number)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
#include "vrdoco.h"
C
      INTEGER IVIEW
      REAL RSTRP, PSTRP
C
C     Local variables
C
C     NRDFQ       Readout strip frequency
C     NSTRP       number of physical strips
C     NSTRR       number of readout strips
C     NOFFS       offset of 1st readout strip from 1st physical strip
C
      INTEGER NRDFQ, NSTRP, NSTRR, NOFFS
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      PSTRP = 0.
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
        VDRSPS = VDERR
      ELSE
C ----------------------------------------------------------------------
        IF (IVIEW .EQ. VVIEWZ) THEN
C  z-side
          NRDFQ = NREFRZ
          NSTRP = NZSTRP
          NSTRR = NRDSTZ
          NOFFS = NOFRDZ
        ELSE
C  rphi-side
          NRDFQ = NREFRP
          NSTRP = NPSTRP
          NSTRR = NRDSTP
          NOFFS = NOFRDP
        ENDIF
C ----------------------------------------------------------------------
C
        IF ((RSTRP .LT. 0.5) .OR. (RSTRP .GT. FLOAT(NSTRR)+0.5)) THEN
          VDRSPS = VDERR
C
        ELSE
C
C  readout strip to physical strip
          PSTRP = (RSTRP - 1.)*FLOAT(NRDFQ) + 1. + NOFFS
          IF ((PSTRP .LT. 0.5) .OR. (PSTRP .GT. FLOAT(NSTRP)+0.5)) THEN
            VDRSPS = VDERR
          ELSE
            VDRSPS  = VDOK
          ENDIF
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C
      RETURN
      END
#endif
