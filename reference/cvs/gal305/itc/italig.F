*DK italig
      SUBROUTINE ITALIG(VECI, VECO)
C.
C...ITALIG  1.01  870714  12:57                          R.Beuselinck
C.
C!  Transform hits to ITC local system (rotated chamber).
C.
C.  Input  VECI(6) /R
C.  Output VECO(6) /R
C.
C.  The elements of VECI,VECO are a position vector (x,y,z) plus
C.  a set of direction cosines (dx/x,dy/y,dz/z).
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "itrotc.h"
      REAL VECI(6),VECO(6)
C
      IF (ITSHFT) THEN
        DO 10 I=1,3
          VECO(I) = ROTITC(1,I)*VECI(1) +
     +              ROTITC(2,I)*VECI(2) +
     +              ROTITC(3,I)*VECI(3) - DXYZIT(I)
          VECO(I+3) = ROTITC(1,I)*VECI(4) +
     +                ROTITC(2,I)*VECI(5) +
     +                ROTITC(3,I)*VECI(6)
   10   CONTINUE
      ELSE
        DO 20 I=1,6
          VECO(I) = VECI(I)
   20   CONTINUE
      ENDIF
      END
#endif
