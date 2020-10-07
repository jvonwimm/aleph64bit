*DK adview
      SUBROUTINE ADVIEW
C------------------------------------------------------------------
C B. BLOCH-DEVAUX  - 850910
C!  Draw two typical cut views of ALEF with header and text
C
C   Called by user   in ASCEVE   from this .HLB
C   Calls  GEANT3 drawing routines
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "gcdraw.h"
      ISEL=111110
      CSIZE=0.4*GPLTRY/20.
C
C   X-Y view
C
      CALL ICLRWK(0,0)
      CALL GDOPT('THRZ','OFF ')
      IGCUT=3
      CALL GDSHOW(1)
      CALL GDXYZ(0)
      CALL GDHEAD(ISEL,'$',CSIZE)
C
C   R-Z view
C
      CALL ICLRWK(0,0)
      CALL GDOPT('THRZ','ON  ')
      IGCUT=1
      CALL GDSHOW(2)
      CALL GDXYZ(0)
      CALL GDHEAD(ISEL,'$',CSIZE)
C
      RETURN
      END
#endif
