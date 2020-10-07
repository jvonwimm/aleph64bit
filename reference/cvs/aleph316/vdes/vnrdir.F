      INTEGER FUNCTION VNRDIR (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns readout direction
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNRDIR / I  Readout direction
C               = +1 if strip channels and electronics channels
C                 are numbered in the same direction
C               = -1 if in opposite directions
C               =  0 if IVIEW is invalid
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
C
      INTEGER  IVIEW
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        VNRDIR = IECORZ
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNRDIR = IECORP
C
      ELSE
C
C     invalid view:
        VNRDIR = 0
      ENDIF
C
      RETURN
      END
#endif
