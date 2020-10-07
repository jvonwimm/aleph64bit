*DK adxy
      SUBROUTINE ADXY(IV)
C------------------------------------------
C   B. BLOCH-DEVAUX 850910
C!    Draw the X-Y cut view of ALEPH in view bank # IV
C
C   Called by    ADINIT , GUVIEW                        from this .HLB
C   Calls  GEANT3 drawing routines
C
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "gcdraw.h"
      CALL GDELET(IV)
      CALL GDOPEN(IV)
      CALL GDRAWC('ALEF',3,0.,GU0,GV0,GSCU,GSCV)
      XTEXT=10.*GPLTRX/20.
      YTEXT=1. *GPLTRY/20.
      CSIZE=0.4*GPLTRY/20.
      CALL GDRAWT(XTEXT,YTEXT,'X-Y FRONT VIEW AT Z=0.$',
     1            CSIZE,0.,2,0)
      CALL GDCLOS
      RETURN
      END
#endif
