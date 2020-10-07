      INTEGER FUNCTION VIMODR (IVIEW,IROM,IMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local module index from readout module and view
C - Steve Wasserbaech, October 1994
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   IROM   / I  Readout module
C
C - Output:
C   VIMODR / I  = VDOK if successful
C               = VDERR if error occurred
C   IMOD   / I  Local module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, IROM, IMOD
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
        IF (LZMULT) THEN
C
C     multiplexing; one readout module per module:
C
          IF (IROM .EQ. 1) THEN
            IMOD = 1
            VIMODR = VDOK
C
          ELSEIF (IROM .EQ. IROMAX) THEN
            IMOD = 2
            VIMODR = VDOK
C
          ELSE
C
C     invalid IROM:
            IMOD = 0
            VIMODR = VDERR
C
          ENDIF
C
        ELSE
C
C     no multiplexing; one readout module per face:
C
          IF ((IROM .GE. 1) .AND. (IROM .LE. NWAFEF)) THEN
C
            IF (IROM .LE. NWAFEM) THEN
              IMOD = 1
            ELSE
              IMOD = 2
            ENDIF
            VIMODR = VDOK
C
          ELSE
C
C     invalid IROM:
            IMOD = 0
            VIMODR = VDERR
C
          ENDIF
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
        IF (IROM .EQ. 1) THEN
          IMOD = 1
          VIMODR = VDOK
C
        ELSEIF (IROM .EQ. IROMAX) THEN
          IMOD = 2
          VIMODR = VDOK
C
        ELSE
C
C     invalid IROM:
          IMOD = 0
          VIMODR = VDERR
        ENDIF
C
      ELSE
C
C     invalid view:
C
        IROM = 0
        VIMODR = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
